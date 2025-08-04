import json
import os
import csv
from dotenv import load_dotenv

# Load the .env file from the V1.0 directory
env_file = os.path.join(os.path.dirname(os.path.dirname(__file__)), '.env')
load_dotenv(env_file)

# Function to load additional information from JSON or TSV
def load_additional_info():
    """Load additional information sources from JSON file (or fall back to TSV)"""
    additional_info = {}
    
    # Try JSON first (preferred format)
    json_path = os.path.join(os.path.dirname(__file__), 'additional_info.json')
    if os.path.exists(json_path):
        try:
            with open(json_path, 'r', encoding='utf-8') as file:
                additional_info = json.load(file)
            print(f"Loaded {len(additional_info)} additional information sources from JSON")
            return additional_info
        except Exception as e:
            print(f"Error loading JSON file: {e}")
    
    # Fall back to TSV for backward compatibility
    tsv_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'data', 'additional_info.tsv')
    if os.path.exists(tsv_path):
        try:
            with open(tsv_path, 'r', encoding='utf-8') as file:
                reader = csv.DictReader(file, delimiter='\t', quoting=csv.QUOTE_NONE)
                for row in reader:
                    if row.get('info_id') and row.get('info_name'):
                        additional_info[row['info_id']] = {
                            'name': row['info_name'],
                            'description': row.get('description', ''),
                            'content': row.get('content', '').replace('\\n', '\n')
                        }
        except Exception as e:
            print(f"Error parsing additional information TSV: {e}")
    
    if not additional_info:
        print("Warning: No additional information sources found")
    
    return additional_info

# Load additional information sources
ADDITIONAL_INFO_SOURCES = load_additional_info()

max_depth = 2
script_name = "election2024_v2"
external_port = 8080
internal_port = 8089
host = '127.0.0.1'

USE_CUSTOM_ENDPOINT = False

DB_NAME = os.getenv('DB_NAME')
DB_USER = os.getenv('DB_USER')
DB_PASSWORD = os.getenv('DB_PASSWORD')
DB_HOST = 'localhost'
DB_PORT = os.getenv('DB_PORT')

OPENAI_API_KEY = os.getenv('OPENAI_API_KEY')
OPENAI_ORGANIZATION = os.getenv('OPENAI_ORGANIZATION')
OPENAI_API_URL = os.getenv('OPENAI_API_URL')
CUSTOM_API_URL = os.getenv('CUSTOM_API_URL')
TOKEN_URL = os.getenv('TOKEN_URL')
CLIENT_ID = os.getenv('CLIENT_ID')
CLIENT_SECRET = os.getenv('CLIENT_SECRET')
SCOPE = os.getenv('SCOPE')

print(f"DB_NAME: {DB_NAME}")
print(f"DB_USER: {DB_USER}")
print(f"DB_HOST: {DB_HOST}")
print(f"DB_PORT: {DB_PORT}")
print(f"USE_CUSTOM_ENDPOINT: {USE_CUSTOM_ENDPOINT}")

error_message = ""
initial_message = "Hi! I'm Brook, a chatbot. I'd like to have a brief conversation with you about the upcoming presidential election.\n\nThis chat should take 8-10 minutes. At any time, you can stop the conversation by clicking the button in the top right corner of the chat.\n\nI'm just a bot, so in order to continue our conversation I'll always need you to respond with something, even if it's short.\n\nSometimes I might get stuck (it may say 'Disconnected' instead of 'Connected' at the top of the chat window). Please refresh the page if this happens! \n\nIs there a name you'd like me to call you?"

# Model configuration
small_model = os.getenv('ELECTIONBOT_MODEL_SMALL', 'gpt-4.1-nano')
large_model = os.getenv('ELECTIONBOT_MODEL_DEFAULT', 'gpt-4.1-mini')
max_tokens = 1500

# Temporary compatibility variables
system_prompt = ""
MYTH1 = ""
MYTH2 = ""
PLACEBO = ""
MYTH1_ARTICLE1 = ""
MYTH1_ARTICLE2 = ""
MYTH2_ARTICLE1 = ""
MYTH2_ARTICLE2 = ""
PLACEBO_ARTICLE1 = ""
PLACEBO_ARTICLE2 = ""

# Load script
print(f"Loading script: {script_name}")
base_dir = os.path.dirname(os.path.abspath(__file__))
data_dir = os.path.join(os.path.dirname(base_dir), 'data')
with open(os.path.join(data_dir, f'{script_name}.json')) as f:
    full_script = json.load(f)
    script_details = full_script[0]
    script_description = script_details['script_description']
    script_length = len(full_script)
    script = full_script[1:script_length]