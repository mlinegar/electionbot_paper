#!/usr/bin/env python3
"""Real-time chat monitoring tool"""
import asyncio
import sys
import os
from datetime import datetime

async def monitor_logs(user_id=None):
    """Monitor chat and debug logs in real-time"""
    if user_id:
        chat_log_pattern = f"chat_logs/{user_id}_*.txt"
        debug_log = f"logging/{user_id}.log"
    else:
        print("Monitoring all new conversations...")
        print("Tip: Pass a user_id as argument to monitor specific conversation")
        print("-" * 80)
    
    # Use tail -f equivalent
    if user_id and os.path.exists(debug_log):
        print(f"Monitoring debug log: {debug_log}")
        proc = await asyncio.create_subprocess_exec(
            'tail', '-f', debug_log,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )
        
        while True:
            line = await proc.stdout.readline()
            if line:
                decoded = line.decode().strip()
                # Highlight important lines
                if 'ERROR' in decoded:
                    print(f"\033[91m{decoded}\033[0m")  # Red
                elif 'WARNING' in decoded:
                    print(f"\033[93m{decoded}\033[0m")  # Yellow
                elif 'my_generate' in decoded:
                    print(f"\033[94m{decoded}\033[0m")  # Blue
                else:
                    print(decoded)
    else:
        print("Please provide a user_id to monitor")

if __name__ == "__main__":
    user_id = sys.argv[1] if len(sys.argv) > 1 else None
    asyncio.run(monitor_logs(user_id))