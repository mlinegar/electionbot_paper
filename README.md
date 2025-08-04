# ElectionBot V1.0 - Paper Repository

This repository contains the original V1.0 implementation of the election chatbot, preserved for code review and academic paper analysis.

## Repository Structure

```
electionbot_paper/
├── chatbot/          # Core V1.0 chatbot implementation
├── web/assets/       # Original web interface files
├── data/             # Data files for chatbot operation
├── analysis/         # R scripts for paper analysis
├── config/           # Configuration files
└── requirements.txt  # Python dependencies
```

## Setup Instructions

### 1. Python Environment

Create and activate a Python virtual environment:

```bash
python3 -m venv venv
source venv/bin/activate  # On Linux/Mac
# or
venv\Scripts\activate  # On Windows
```

Install dependencies:

```bash
pip install -r requirements.txt
```

### 2. Database Setup

The chatbot uses PostgreSQL. Set up the database:

```bash
# Create database
createdb electionbot_v1

# Run database migrations
cd chatbot
python manage_db.py
```

### 3. Configuration

Update the configuration in `chatbot/config.py`:
- Database connection settings
- API keys (OpenAI, etc.)
- WebSocket server settings

### 4. Running the Chatbot

Start the chat server:

```bash
cd chatbot
python launch_chatserver.py
```

Start the web application:

```bash
python launch_web_app.py
```

### 5. Web Server Configuration (Nginx)

Example Nginx configuration for production deployment:

```nginx
server {
    listen 80;
    server_name your-domain.com;

    location / {
        proxy_pass http://localhost:5000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    location /ws {
        proxy_pass http://localhost:8765;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_read_timeout 86400;
    }
}
```

### 6. Process Management (Supervisor)

Use the provided supervisor configuration in `config/electionbot.supervisor.conf` to manage the chatbot processes.

## Data Files

- `data/additional_info.tsv` - Additional information for chatbot responses
- `data/rumor_examples.tsv` - Example rumors/misinformation
- `data/election2024*.json` - Election-specific data files
- `data/non_citizen_voting*.json` - Topic-specific response data

## Analysis Code

The `analysis/` directory contains R scripts for paper analysis:
- `electionbot.R` - Main analysis script
- `helper_functions.R` - Utility functions for analysis

## Key Components

### Chatbot Core (`chatbot/`)
- `chat_logic.py` - Main chat logic implementation
- `my_generate.py` - Response generation logic
- `handlers*.py` - Request handlers for different pages
- `models.py` - Database models
- `session_manager.py` - Session management
- `utils.py` - Utility functions

### Web Interface (`web/assets/`)
- `index_optimized_generalized.html` - Main chat interface
- `domains_selection.html` - Domain selection page
- `survey-complete.html` - Post-chat completion page
- `script.js` - Client-side JavaScript
- `index.css` - Styling

## Academic Context

This is the V1.0 implementation used for the research collaboration between Caltech and Washington University on misinformation inoculation through conversational AI.

## Important Notes

- This is a preserved version for academic review and should not be modified
- The implementation uses WebSocket for real-time chat communication
- Multiple research domains are supported (elections, vaccines, AI)
- Integration with YouGov survey platform for research data collection

## Troubleshooting

1. **Database connection issues**: Check PostgreSQL is running and credentials in `config.py` are correct
2. **WebSocket errors**: Ensure both chat server (port 8765) and web app (port 5000) are running
3. **Missing dependencies**: Run `pip install -r requirements.txt` again

## Citation

[Add paper citation details when available]