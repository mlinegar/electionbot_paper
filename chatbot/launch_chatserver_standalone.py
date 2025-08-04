#!/usr/bin/env python3
import asyncio
import json
import os
import sys
import uuid
from datetime import datetime
from urllib.parse import parse_qs
from aiohttp import web

# Add current directory to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from config import external_port, host, initial_message
from chat_logic import WebSocketExecutor
from database import engine
from models import Base

def create_tables():
    Base.metadata.create_all(bind=engine)

class ChatServer:
    def __init__(self, port=8081, host='127.0.0.1'):
        self.port = port
        self.host = host
        self.executors = []
        print(f"Initializing ChatServer on {host}:{port}")
    
    async def handler(self, request):
        ws = web.WebSocketResponse()
        await ws.prepare(request)
        
        # Get user ID from query params
        query = parse_qs(request.query_string)
        user_id = query.get('user_id', [str(uuid.uuid4())])[0]
        
        print(f"New WebSocket connection from user: {user_id}")
        
        # Create executor for this connection
        executor = WebSocketExecutor(
            session_id=user_id,
            websocket=ws,
            initial_message=initial_message
        )
        self.executors.append(executor)
        
        try:
            # Start the conversation
            await executor.run()
        except Exception as e:
            print(f"WebSocket error: {e}")
        finally:
            # Clean up
            if executor in self.executors:
                self.executors.remove(executor)
            await ws.close()
            print(f"WebSocket connection closed for user: {user_id}")
        
        return ws
    
    def run(self):
        app = web.Application()
        app.router.add_get('/ws', self.handler)
        
        print(f"Starting V1.0 ChatServer on {self.host}:{self.port}")
        web.run_app(app, host=self.host, port=self.port)

if __name__ == "__main__":
    print("Starting ElectionBot V1.0 server...")
    create_tables()
    chatserver = ChatServer(port=8081, host='127.0.0.1')
    chatserver.run()