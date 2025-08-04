#!/bin/bash
# Run current ElectionBot (restore from V1.0)

echo "Stopping any running ElectionBot servers..."
pkill -f "launch_chatserver.py" 2>/dev/null || true
sleep 2

echo "Restoring current assets..."
cd /home/mlinegar/electionbot/chat
if [ -L "assets" ]; then
    rm assets
fi
if [ -d "assets.current" ]; then
    mv assets.current assets
fi

echo "Starting current ElectionBot..."
cd /home/mlinegar/electionbot
.ebc/bin/python launch_chatserver.py