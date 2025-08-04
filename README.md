# The Persuasiveness of Artificial Intelligence: Can AI Chatbots Reduce Beliefs in Election Rumors

This repository contains the replication code for our paper examining the effectiveness of AI chatbots in combating election misinformation.

## Overview

This project implements an AI-powered chatbot designed to engage users in conversations about election integrity, providing fact-based information to counter common election-related misinformation. The chatbot was deployed as part of a randomized controlled trial to measure its effectiveness in reducing belief in election rumors.

## Repository Structure

```
electionbot_paper/
├── analysis/                 # Statistical analysis code
│   ├── electionbot.R        # Main analysis script
│   └── helper_functions.R   # Helper functions for analysis
├── chatbot/                 # Core chatbot implementation
├── web/assets/              # Web interface files
├── data/                    # Data files and conversation scripts
├── config/                  # Configuration files
└── requirements.txt         # Python dependencies
```

## Analysis Code

The statistical analysis examining the chatbot's effectiveness is contained in the `analysis/` directory.

### Running the Analysis

1. Open R or RStudio
2. Set your working directory to the `analysis/` folder
3. Run the main analysis script:
   ```r
   source("electionbot.R")
   ```

The analysis script will:
- Load the helper functions from `helper_functions.R` automatically
- Process conversation logs and survey data
- Generate tables and figures showing treatment effects
- Output results comparing chatbot effectiveness to control conditions

### Key Analysis Components

- **`electionbot.R`**: Main analysis script that produces all results reported in the paper
- **`helper_functions.R`**: Supporting functions for data processing and statistical tests

## Chatbot Implementation

### Prerequisites

- Python 3.8 or higher
- PostgreSQL 12 or higher
- pip (Python package manager)
- An OpenAI API key (for GPT models)
- (Optional) Nginx for production deployment

### Detailed Setup Instructions

#### 1. Clone the Repository

```bash
git clone https://github.com/mlinegar/electionbot_paper.git
cd electionbot_paper
```

#### 2. Set Up Python Environment

Create and activate a virtual environment:

```bash
# Create virtual environment
python3 -m venv venv

# Activate it
# On Linux/Mac:
source venv/bin/activate

# On Windows:
# venv\Scripts\activate
```

Install Python dependencies:

```bash
pip install -r requirements.txt
```

#### 3. Install and Configure PostgreSQL

**On Ubuntu/Debian:**
```bash
sudo apt update
sudo apt install postgresql postgresql-contrib
```

**On Mac (using Homebrew):**
```bash
brew install postgresql
brew services start postgresql
```

**Create a database and user:**
```bash
# Switch to postgres user
sudo -u postgres psql

# In the PostgreSQL prompt, run:
CREATE DATABASE electionbot_db;
CREATE USER electionbot_user WITH PASSWORD 'your_secure_password';
GRANT ALL PRIVILEGES ON DATABASE electionbot_db TO electionbot_user;
\q
```

#### 4. Configure Environment Variables

Copy the example environment file and edit it:

```bash
cd /path/to/electionbot_paper
cp .env.example .env
nano .env  # or use your preferred text editor
```

Update the values in `.env` with your actual credentials:

```bash
# Database Configuration
DB_NAME=electionbot_db
DB_USER=electionbot_user
DB_PASSWORD=your_secure_password
DB_HOST=localhost
DB_PORT=5432

# OpenAI Configuration
OPENAI_API_KEY=your-openai-api-key-here
OPENAI_ORGANIZATION=org-your-org-id  # Optional

# Server Configuration
EXTERNAL_PORT=8080
INTERNAL_PORT=8081

# Model Configuration (Optional - defaults shown)
ELECTIONBOT_MODEL_SMALL=gpt-3.5-turbo
ELECTIONBOT_MODEL_DEFAULT=gpt-4
```

#### 5. Initialize the Database

```bash
# Navigate to chatbot directory
cd chatbot

# Run database initialization
python manage_db.py

# You should see output like:
# Creating database tables...
# Database initialized successfully!
```

#### 6. Verify the Installation

Check that the configuration loads correctly:

```bash
cd chatbot
python -c "import config; print('Configuration loaded successfully!')"
```

## Running the Chatbot

### For Local Development/Testing

Simply run the Python script directly:

```bash
cd /path/to/electionbot_paper/chatbot
python launch_chatserver.py
```

You should see:
```
Starting ElectionBot server...
Loading script: election2024_v2
Server running on http://127.0.0.1:8081
```

Then open your browser to `http://localhost:8081/bot`

To stop: Press `Ctrl+C`

### For Production Deployment

For production, we use Nginx as a reverse proxy and Supervisor to manage the process. This provides:
- HTTPS support
- Automatic restart on crashes
- Better performance
- Professional deployment

#### Step 1: Install and Configure Nginx

```bash
sudo apt install nginx
sudo nano /etc/nginx/sites-available/electionbot
```

Add this configuration (replace `your-domain.com` with your actual domain):

```nginx
server {
    listen 80;
    server_name your-domain.com;
    
    location / {
        proxy_pass http://127.0.0.1:8081;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_read_timeout 86400;
    }
}
```

Enable the site:
```bash
sudo ln -s /etc/nginx/sites-available/electionbot /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

#### Step 2: Set Up Supervisor (keeps the bot running)

```bash
sudo apt install supervisor
sudo cp config/electionbot.supervisor.conf /etc/supervisor/conf.d/
sudo supervisorctl reread
sudo supervisorctl update
sudo supervisorctl start electionbot
```

The bot will now:
- Start automatically on server boot
- Restart if it crashes
- Log output to `/var/log/supervisor/`

### Configuration

#### Main Configuration File

The primary configuration is in `chatbot/config_standalone.py`:

```python
# Key configuration variables:
script_name = "election2024_v2"  # Which conversation script to use
max_depth = 2                    # Maximum conversation depth
external_port = 8080            # Port for external access
internal_port = 8081            # Internal server port
host = '127.0.0.1'              # Server host

# Model configuration
small_model = 'gpt-3.5-turbo'   # For simple responses
large_model = 'gpt-4'            # For complex reasoning
max_tokens = 1500                # Maximum response length
```

#### Available Scripts

- `election2024_v2.json` - Main election conversation covering voter fraud claims
- `non_citizen_voting_v7.json` - Focused on non-citizen voting myths
- `election2024_v3_streamlined.json` - Shorter, more focused version

To change scripts, edit `script_name` in the config file.

## Conversation Script Format

### Script Structure

The chatbot uses a structured conversation format with steps and questions. Each script is a JSON array with:

1. **Header object** - Contains metadata
2. **Step objects** - Each step has multiple questions

### Question Types and Special Syntax

#### ASK: Directive
When a question contains `"ASK:"`, the chatbot must ask that specific question:
```json
{
  "question_text_every_time": "ASK: Would you like a refresher about claims of election fraud?",
  "generate_flag": true,
  "flag_text": "Does the user want a refresher?"
}
```

#### VERBATIM Responses
When `"verbatim": true`, the chatbot outputs the exact text without modification:
```json
{
  "question_text_first_time": "Thank you for participating!",
  "verbatim": true,
  "requires_user_answer": false
}
```

### Flags and Branching

The conversation flow is controlled by flags that the chatbot sets based on user responses:

```json
{
  "generate_flag": true,
  "flag_text": "Does the user have any follow up questions?",
  "flag_required": true
}
```

- `generate_flag`: Whether to evaluate and set a flag
- `flag_text`: The question the chatbot answers to set the flag
- `flag_required`: Whether a specific flag value is needed to show this question

#### Flag Logic
- Flags are boolean (true/false)
- The chatbot evaluates user responses against `flag_text`
- Questions with `"flag_required": true` only appear if the previous flag was true
- Questions with `"flag_required": false` only appear if the previous flag was false

### Important Fields

- `question_text_first_time` - Text used on first encounter
- `question_text_every_time` - Text used if revisiting (e.g., after a loop)
- `requires_user_answer` - Whether to wait for user input
- `allow_assistant_response` - Whether the assistant can respond
- `max_length` - Maximum tokens for the response
- `recallable` - Whether this node can be returned to

### Example Flow

```json
[
  {
    "step_num": 0,
    "questions": [
      {
        "question_id": "intro",
        "question_text_first_time": "Introduce the topic of election fraud",
        "question_text_every_time": "ASK: Would you like a refresher?",
        "generate_flag": true,
        "flag_text": "Does the user want a refresher?",
        "flag_required": null
      },
      {
        "question_id": "detail",
        "question_text_first_time": "Provide detailed information...",
        "flag_required": true,  // Only shows if previous flag was true
        "verbatim": false
      }
    ]
  }
]
```

## Additional Information Sources

The chatbot can reference fact-checking sources defined in `data/additional_info.tsv`:

```tsv
info_id	info_name	description	content
source1	AP Fact Check	Associated Press fact-checking	Detailed fact-check content...
source2	Reuters	Reuters fact-checking service	Fact-check findings...
```

These are loaded automatically and can be referenced by the chatbot when providing evidence-based corrections.

## Experimental Design

The chatbot was designed to:
1. Engage users in natural conversation about elections
2. Identify and address specific misinformation beliefs
3. Provide fact-based corrections from authoritative sources
4. Measure changes in beliefs through pre/post surveys

## Citation

If you use this code in your research, please cite:

```bibtex
@article{[citation details to be added upon publication]}
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact

For questions about the code or implementation, please contact [contact information].

## Acknowledgments

This research was conducted in collaboration between Caltech and Washington University in St. Louis.