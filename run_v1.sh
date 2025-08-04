#!/bin/bash
# Run ElectionBot with V1.0 assets

echo "Stopping any running ElectionBot servers..."
pkill -f "launch_chatserver.py" 2>/dev/null || true
sleep 2

echo "Setting up V1.0 assets..."
cd /home/mlinegar/electionbot/chat
if [ -d "assets" ] && [ ! -L "assets" ]; then
    mv assets assets.current
fi
ln -sfn /home/mlinegar/electionbot_paper/web/assets assets

echo "Starting ElectionBot V1.0..."
cd /home/mlinegar/electionbot
.ebc/bin/python launch_chatserver.py