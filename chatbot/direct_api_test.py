#!/usr/bin/env python3
"""Test OpenAI API directly with working key"""
import asyncio
import json
import aiohttp
import os

async def test_direct_openai():
    """Test OpenAI API directly"""
    
    api_key = os.getenv("OPENAI_API_KEY", "your-api-key-here")
    
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    }
    
    payload = {
        "model": "gpt-4o-mini",
        "messages": [
            {"role": "user", "content": "Say exactly: TEST RESPONSE"}
        ],
        "temperature": 0,
        "max_tokens": 10
    }
    
    print("Testing OpenAI API directly (without retries)...")
    
    try:
        async with aiohttp.ClientSession() as session:
            async with session.post(
                "https://api.openai.com/v1/chat/completions",
                headers=headers,
                json=payload
            ) as response:
                print(f"Status Code: {response.status}")
                
                if response.status == 200:
                    result = await response.json()
                    print(f"✅ Success!")
                    print(f"Response: {json.dumps(result, indent=2)}")
                    
                    if 'choices' in result and result['choices']:
                        content = result['choices'][0]['message']['content']
                        print(f"Content: '{content}'")
                        return content
                else:
                    error_text = await response.text()
                    print(f"❌ Error {response.status}: {error_text}")
                    return None
                    
    except Exception as e:
        print(f"❌ Exception: {type(e).__name__}: {e}")
        return None

if __name__ == "__main__":
    asyncio.run(test_direct_openai())