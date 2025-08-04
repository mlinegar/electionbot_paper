#!/usr/bin/env python3
"""Check recent chat logs for errors and empty responses"""
import os
from datetime import datetime
import glob

def check_recent_logs():
    # Find the most recent chat log
    chat_logs = glob.glob("chat_logs/*.txt")
    if not chat_logs:
        print("No chat logs found!")
        return
    
    # Sort by modification time
    chat_logs.sort(key=lambda x: os.path.getmtime(x), reverse=True)
    
    print(f"Found {len(chat_logs)} chat logs")
    print("\nMost recent 5 conversations:")
    print("-" * 80)
    
    for log_file in chat_logs[:5]:
        mtime = datetime.fromtimestamp(os.path.getmtime(log_file))
        user_id = os.path.basename(log_file).split('_')[0]
        
        # Read the log file
        with open(log_file, 'r') as f:
            content = f.read()
        
        # Check for empty assistant responses
        lines = content.split('\n')
        empty_responses = []
        for i, line in enumerate(lines):
            if line.strip().startswith("assistant:") and i > 0:
                # Check if the assistant message is empty or very short
                assistant_content = line.replace("assistant:", "").strip()
                if len(assistant_content) < 5:
                    empty_responses.append(i)
        
        print(f"\nFile: {os.path.basename(log_file)}")
        print(f"User ID: {user_id}")
        print(f"Modified: {mtime}")
        print(f"Total lines: {len(lines)}")
        print(f"Empty responses: {len(empty_responses)}")
        
        if empty_responses:
            print("Empty response line numbers:", empty_responses[:5], "..." if len(empty_responses) > 5 else "")
        
        # Check corresponding debug log
        debug_log = f"logging/{user_id}.log"
        if os.path.exists(debug_log):
            with open(debug_log, 'r') as f:
                debug_content = f.read()
            
            # Look for errors
            error_count = debug_content.count("ERROR")
            warning_count = debug_content.count("WARNING")
            
            print(f"Debug log: {error_count} errors, {warning_count} warnings")
            
            # Show last few errors
            if error_count > 0:
                error_lines = [line for line in debug_content.split('\n') if 'ERROR' in line]
                print("Last error:", error_lines[-1] if error_lines else "None")

if __name__ == "__main__":
    check_recent_logs()