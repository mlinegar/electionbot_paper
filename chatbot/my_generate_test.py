import asyncio
import os
import aiohttp
import json
from typing import List, Dict, Any, Optional
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

OPENAI_API_URL = 'https://api.openai.com/v1/chat/completions'
CUSTOM_API_URL = 'https://api.openai.wustl.edu/base-gpt-4o-128k/v1/chat/completions'

# Configuration for custom endpoint
TOKEN_URL = os.getenv('TOKEN_URL')
CLIENT_ID = os.getenv('CLIENT_ID')
CLIENT_SECRET = os.getenv('CLIENT_SECRET')
SCOPE = os.getenv('SCOPE')
OPENAI_API_KEY = os.getenv('OPENAI_API_KEY')

def format_prompt(messages: List[Dict[str, str]], model_name: str) -> List[Dict[str, str]]:
    if "gpt" in model_name.lower():
        return messages
    else:
        return [{"role": message["role"], "content": message["content"]} for message in messages]

class APIError(Exception):
    def __init__(self, status_code: int, message: str):
        self.status_code = status_code
        self.message = message
        super().__init__(f"API Error {status_code}: {message}")

class OpenAIClient:
    def __init__(self, api_key: str, base_url: str = OPENAI_API_URL):
        self.api_key = api_key
        self.base_url = base_url

    async def create_chat_completion(self, **kwargs):
        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self.api_key}"
        }

        async with aiohttp.ClientSession() as session:
            async with session.post(self.base_url, headers=headers, json=kwargs) as response:
                if response.status != 200:
                    error_text = await response.text()
                    raise APIError(response.status, error_text)
                
                if kwargs.get('stream', False):
                    async for line in response.content:
                        if line.startswith(b'data: '):
                            if line.strip() == b'data: [DONE]':
                                break
                            try:
                                chunk = json.loads(line.decode('utf-8').split('data: ')[1])
                                yield chunk
                            except json.JSONDecodeError:
                                continue
                else:
                    yield await response.json()

class CustomClient:
    def __init__(self, token_url: str, client_id: str, client_secret: str, scope: str, base_url: str = CUSTOM_API_URL):
        self.token_url = token_url
        self.client_id = client_id
        self.client_secret = client_secret
        self.scope = scope
        self.base_url = base_url
        self.access_token = None

    async def get_access_token(self):
        async with aiohttp.ClientSession() as session:
            payload = {
                'grant_type': 'client_credentials',
                'scope': self.scope
            }
            auth = aiohttp.BasicAuth(self.client_id, self.client_secret)
            async with session.post(self.token_url, data=payload, auth=auth) as response:
                if response.status != 200:
                    error_text = await response.text()
                    raise APIError(response.status, f"Failed to get access token: {error_text}")
                token_data = await response.json()
                return token_data['access_token']

    async def create_chat_completion(self, **kwargs):
        if not self.access_token:
            self.access_token = await self.get_access_token()
            # Update the token in the .env file
            self.update_env_token(self.access_token)

        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self.access_token}"
        }

        async with aiohttp.ClientSession() as session:
            async with session.post(self.base_url, headers=headers, json=kwargs) as response:
                if response.status == 401:
                    # Token might have expired, try to get a new one
                    self.access_token = await self.get_access_token()
                    # Update the token in the .env file
                    self.update_env_token(self.access_token)
                    headers["Authorization"] = f"Bearer {self.access_token}"
                    async with session.post(self.base_url, headers=headers, json=kwargs) as retry_response:
                        if retry_response.status != 200:
                            error_text = await retry_response.text()
                            raise APIError(retry_response.status, error_text)
                        async for chunk in self.process_response(retry_response, kwargs.get('stream', False)):
                            yield chunk
                elif response.status != 200:
                    error_text = await response.text()
                    raise APIError(response.status, error_text)
                else:
                    async for chunk in self.process_response(response, kwargs.get('stream', False)):
                        yield chunk

    async def process_response(self, response, stream):
        if stream:
            async for line in response.content:
                if line.startswith(b'data: '):
                    if line.strip() == b'data: [DONE]':
                        break
                    try:
                        chunk = json.loads(line.decode('utf-8').split('data: ')[1])
                        yield chunk
                    except json.JSONDecodeError:
                        continue
        else:
            yield await response.json()

    def update_env_token(self, token):
        env_path = '.env'
        with open(env_path, 'r') as file:
            lines = file.readlines()
        
        with open(env_path, 'w') as file:
            for line in lines:
                if line.startswith('CUSTOM_API_TOKEN='):
                    file.write(f'CUSTOM_API_TOKEN={token}\n')
                else:
                    file.write(line)

async def my_generate(messages: List[Dict[str, str]], model: str = "gpt-4.1-nano", temperature: float = 0.7, 
                      max_tokens: int = 256, top_p: float = 1.0, frequency_penalty: float = 0.0, 
                      presence_penalty: float = 0.0, schema: Optional[Dict[str, Any]] = None, 
                      stream: bool = False, use_custom_endpoint: bool = False):
    formatted_messages = format_prompt(messages, model)
    collected_text = ""

    if use_custom_endpoint:
        client = CustomClient(TOKEN_URL, CLIENT_ID, CLIENT_SECRET, SCOPE)
    else:
        client = OpenAIClient(OPENAI_API_KEY)

    try:
        kwargs = {
            "model": model,
            "messages": formatted_messages,
            "temperature": temperature,
            "max_tokens": max_tokens,
            "top_p": top_p,
            "frequency_penalty": frequency_penalty,
            "presence_penalty": presence_penalty,
            "stream": stream
        }

        if schema is not None:
            kwargs["functions"] = [{"name": "json_test", "parameters": schema}]
            kwargs["function_call"] = "auto"

        async for chunk in client.create_chat_completion(**kwargs):
            if stream:
                if 'choices' in chunk and chunk['choices']:
                    choice = chunk['choices'][0]
                    if 'delta' in choice and 'content' in choice['delta']:
                        chunk_content = choice['delta']['content']
                        collected_text += chunk_content
                        yield chunk_content
            else:
                if 'choices' in chunk and chunk['choices']:
                    message = chunk['choices'][0]['message']
                    if 'content' in message and message['content'] is not None:
                        collected_text = message['content']
                    elif 'function_call' in message and message['function_call'] and 'arguments' in message['function_call']:
                        collected_text = message['function_call']['arguments']
                    else:
                        raise APIError(500, "Unexpected response structure or content missing.")
                else:
                    raise APIError(500, "Response does not contain 'choices'.")
                
                yield collected_text.strip()

        if stream and not collected_text:
            yield collected_text.strip()

    except APIError as e:
        yield f"API Error {e.status_code}: {e.message}"
    except Exception as e:
        yield f"Unexpected error: {str(e)}"

# Test function
async def test_my_generate():
    messages = [
        {"role": "system", "content": "You are a helpful assistant."},
        {"role": "user", "content": "What is the first letter of the alphabet?"}
    ]
    
    print("Testing OpenAI endpoint:")
    async for response in my_generate(messages):
        print(f"Response: {response}")

    print("\nTesting custom endpoint:")
    async for response in my_generate(messages, use_custom_endpoint=True):
        print(f"Response: {response}")

# Run the test function
if __name__ == "__main__":
    asyncio.run(test_my_generate())