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

- Python 3.8+
- PostgreSQL database
- Nginx (for production deployment)

### Setup Instructions

1. **Create Python Virtual Environment**
   ```bash
   python3 -m venv venv
   source venv/bin/activate  # On Linux/Mac
   ```

2. **Install Dependencies**
   ```bash
   pip install -r requirements.txt
   ```

3. **Database Setup**
   ```bash
   # Create PostgreSQL database
   createdb electionbot_v1
   
   # Update database credentials in .env file
   cp .env.example .env
   # Edit .env with your database credentials
   ```

4. **Initialize Database**
   ```bash
   cd chatbot
   python manage_db.py
   ```

### Running the Chatbot

#### Development Mode

Start the chatbot server:
```bash
cd chatbot
python launch_chatserver.py
```

The chatbot will be available at `http://localhost:8081`

#### Production Deployment

1. **Configure Nginx**
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
       }
   }
   ```

2. **Use Supervisor for Process Management**
   ```bash
   # Copy supervisor config
   sudo cp config/electionbot.supervisor.conf /etc/supervisor/conf.d/
   sudo supervisorctl reread
   sudo supervisorctl update
   sudo supervisorctl start electionbot
   ```

### Configuration

Key configuration options in `chatbot/config.py`:
- `script_name`: Selects which conversation script to use (default: "election2024_v2")
- `external_port`: External port for web interface
- `internal_port`: Internal WebSocket port
- Database connection settings
- OpenAI API configuration

### Conversation Scripts

The chatbot uses structured conversation scripts located in `data/`:
- `election2024_v2.json`: Main conversation flow about election integrity
- `rumor_examples.tsv`: Examples of election misinformation
- `additional_info.tsv`: Supplementary information sources

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