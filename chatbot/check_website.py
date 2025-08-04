import requests
import subprocess
import logging
from datetime import datetime
import os
from pathlib import Path
import sys

# Configure logging to both file and console
HOME = Path.home()
log_dir = HOME / "logs" / "website-monitor"
log_dir.mkdir(parents=True, exist_ok=True)
log_file = log_dir / "website-monitor.log"

# Create a console handler
console_handler = logging.StreamHandler(sys.stdout)
console_handler.setLevel(logging.INFO)
console_formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
console_handler.setFormatter(console_formatter)

# Create a file handler
file_handler = logging.FileHandler(str(log_file))
file_handler.setLevel(logging.INFO)
file_formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
file_handler.setFormatter(file_formatter)

# Set up the logger
logger = logging.getLogger('website_monitor')
logger.setLevel(logging.INFO)
logger.addHandler(console_handler)
logger.addHandler(file_handler)

def check_website(url="https://electionbot.chat"):
    """Check if the website is responding properly"""
    try:
        logger.info(f"Checking website at {url}")
        # Add headers to mimic a browser request
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
        }
        response = requests.get(url, timeout=10, headers=headers, verify=True)
        if response.status_code == 200:
            logger.info("Website is up and running")
            return True
        else:
            logger.error(f"Website returned status code: {response.status_code}")
            logger.error(f"Response content: {response.text[:200]}...")  # Log first 200 chars of response
            return False
    except requests.RequestException as e:
        logger.error(f"Error checking website: {str(e)}")
        return False

def restart_service():
    """Restart the chatbot service using supervisorctl"""
    try:
        logger.info("Attempting to restart chatbot service...")
        result = subprocess.run(
            ["sudo", "supervisorctl", "restart", "chatbot"], 
            capture_output=True, 
            text=True
        )
        if result.returncode == 0:
            logger.info("Successfully restarted chatbot service")
            return True
        else:
            logger.error(f"Failed to restart service: {result.stderr}")
            return False
    except subprocess.SubprocessError as e:
        logger.error(f"Error restarting service: {str(e)}")
        return False

def main():
    logger.info("Starting website health check")
    if not check_website():
        logger.warning("Website is down, attempting to restart service")
        if restart_service():
            # Wait a few seconds for the service to start up
            import time
            logger.info("Waiting 10 seconds for service to start...")
            time.sleep(10)
            
            # Check if the website is back up
            if check_website():
                logger.info("Website successfully restored")
            else:
                logger.error("Website still down after restart attempt")
        else:
            logger.error("Failed to restart service")
    else:
        logger.info("Health check completed successfully")

if __name__ == "__main__":
    main()