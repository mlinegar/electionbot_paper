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

#### 6. Test the Installation

Run a simple test to ensure everything is connected:

```bash
# Still in the chatbot directory
python -c "from database import engine; print('Database connection successful!')"
```

### Running the Chatbot

#### Development Mode (Local Testing)

1. **Start the chatbot server:**
   ```bash
   cd /path/to/electionbot_paper/chatbot
   python launch_chatserver.py
   ```

   You should see:
   ```
   Starting ElectionBot server...
   Loading script: election2024_v2
   Server initialized in X.XXs
   Server running on http://127.0.0.1:8081
   ```

2. **Access the chatbot:**
   - Open your web browser
   - Navigate to `http://localhost:8081/bot`
   - You should see the ElectionBot interface

3. **To stop the server:**
   - Press `Ctrl+C` in the terminal

#### Common Issues and Troubleshooting

**Issue: "psycopg2" installation fails**
```bash
# Install PostgreSQL development files
# On Ubuntu/Debian:
sudo apt-get install libpq-dev python3-dev

# On Mac:
brew install postgresql
```

**Issue: "OpenAI API key is invalid"**
- Ensure your API key in `.env` starts with `sk-`
- Check that you have credits on your OpenAI account
- Verify the key at https://platform.openai.com/api-keys

**Issue: "Database connection failed"**
```bash
# Check PostgreSQL is running
sudo systemctl status postgresql  # Linux
brew services list  # Mac

# Test connection
psql -U electionbot_user -d electionbot_db -h localhost
```

**Issue: "Port 8081 already in use"**
```bash
# Find and kill the process using the port
lsof -i :8081
kill -9 <PID>
```

### Production Deployment

#### Using Nginx and Supervisor

1. **Install Nginx:**
   ```bash
   sudo apt install nginx  # Ubuntu/Debian
   ```

2. **Create Nginx configuration:**
   ```bash
   sudo nano /etc/nginx/sites-available/electionbot
   ```

   Add:
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
           proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
           proxy_set_header X-Forwarded-Proto $scheme;
           proxy_read_timeout 86400;
       }
   }
   ```

3. **Enable the site:**
   ```bash
   sudo ln -s /etc/nginx/sites-available/electionbot /etc/nginx/sites-enabled/
   sudo nginx -t  # Test configuration
   sudo systemctl reload nginx
   ```

4. **Install and configure Supervisor:**
   ```bash
   sudo apt install supervisor
   
   # Copy the provided supervisor config
   sudo cp config/electionbot.supervisor.conf /etc/supervisor/conf.d/
   
   # Edit the config to match your paths
   sudo nano /etc/supervisor/conf.d/electionbot.supervisor.conf
   
   # Reload supervisor
   sudo supervisorctl reread
   sudo supervisorctl update
   sudo supervisorctl start electionbot
   ```

5. **Check status:**
   ```bash
   sudo supervisorctl status electionbot
   ```

### Configuration Options

The main configuration file is `chatbot/config.py`. Key options:

- `script_name`: Which conversation script to use
  - `"election2024_v2"` (default): Main election conversation
  - `"non_citizen_voting_v7"`: Focused on non-citizen voting myths
  
- `max_depth`: Maximum conversation depth (default: 2)
- `small_model` / `large_model`: GPT model names
- `max_tokens`: Maximum response length

### Testing the Installation

Once running, test the chatbot:

1. Navigate to the chat interface
2. You should see: "Hi! I'm Brook, a chatbot..."
3. Type a response and press Enter
4. The bot should respond within a few seconds

Check logs for errors:
```bash
# If using supervisor
sudo tail -f /var/log/supervisor/electionbot-stdout.log

# If running directly
# Check the terminal where you started the server
```

## Data Files

### Conversation Scripts

Located in `data/`:
- `election2024_v2.json`: Main conversation flow
- `rumor_examples.tsv`: Election misinformation examples
- `additional_info.tsv`: Fact-checking sources

### Script Format

The conversation scripts are JSON files with this structure:
```json
[
  {
    "script_description": "Description of the conversation flow",
    "initial_message": "Hi! I'm Brook..."
  },
  {
    "node_id": "ask_name",
    "text": "Is there a name you'd like me to call you?",
    "children": ["greeting"]
  }
]
```

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