#!/usr/bin/env python3
"""
Static/Interactive Election Misinformation Inoculation Web Application
"""
import asyncio
import json
import os
from pathlib import Path
from typing import List, Dict, Any, Optional
from aiohttp import web, ClientSession
from aiohttp.web import Request, Response, json_response
from aiohttp_cors import setup as cors_setup, ResourceOptions
import aiofiles
import uuid
from datetime import datetime

from base.config_modular import ConfigManager, ContentPiece, InoculationConfig
from base.my_generate import my_generate

class InoculationApp:
    """Main web application for election misinformation inoculation"""
    
    def __init__(self):
        self.config_manager = ConfigManager()
        self.app = web.Application()
        self.setup_routes()
        self.setup_cors()
        self.upload_dir = Path("uploads")
        self.upload_dir.mkdir(exist_ok=True)
    
    def setup_cors(self):
        """Setup CORS for frontend access"""
        cors = cors_setup(self.app, defaults={
            "*": ResourceOptions(
                allow_credentials=True,
                expose_headers="*",
                allow_headers="*",
                allow_methods="*"
            )
        })
        
        # Add CORS to all routes
        for route in list(self.app.router.routes()):
            cors.add(route)
    
    def setup_routes(self):
        """Setup application routes"""
        # Static content
        self.app.router.add_get('/', self.index)
        self.app.router.add_static('/static/', path='static', name='static')
        
        # API endpoints
        self.app.router.add_get('/api/content/list', self.list_content)
        self.app.router.add_get('/api/content/{content_type}', self.get_content_by_type)
        self.app.router.add_post('/api/content/upload', self.upload_content)
        self.app.router.add_post('/api/inoculation/generate', self.generate_inoculation)
        self.app.router.add_post('/api/inoculation/chat', self.chat_with_inoculation)
        
        # Configuration management
        self.app.router.add_get('/api/config/export', self.export_config)
        self.app.router.add_post('/api/config/import', self.import_config)
    
    async def index(self, request: Request) -> Response:
        """Serve the main application page"""
        html_content = """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Election Misinformation Inoculation Tool</title>
    <style>
        body { font-family: Arial, sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }
        .section { margin: 30px 0; padding: 20px; border: 1px solid #ddd; border-radius: 8px; }
        .section h2 { color: #333; border-bottom: 2px solid #007bff; padding-bottom: 10px; }
        .form-group { margin: 15px 0; }
        label { display: block; margin-bottom: 5px; font-weight: bold; }
        input, select, textarea { width: 100%; padding: 8px; margin-bottom: 10px; border: 1px solid #ddd; border-radius: 4px; }
        textarea { height: 100px; }
        button { background-color: #007bff; color: white; padding: 10px 20px; border: none; border-radius: 4px; cursor: pointer; }
        button:hover { background-color: #0056b3; }
        .content-list { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 15px; }
        .content-item { padding: 15px; border: 1px solid #eee; border-radius: 4px; background-color: #f9f9f9; }
        .content-item h4 { margin: 0 0 10px 0; color: #007bff; }
        .inoculation-output { background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 20px; }
        .chat-container { max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 15px; margin: 10px 0; }
        .message { margin: 10px 0; padding: 10px; border-radius: 8px; }
        .user-message { background-color: #e3f2fd; text-align: right; }
        .assistant-message { background-color: #f1f8e9; }
    </style>
</head>
<body>
    <h1>🗳️ Election Misinformation Inoculation Tool</h1>
    <p>Create personalized inoculation doses against election misinformation by combining myths, factual articles, and additional information.</p>

    <!-- Content Upload Section -->
    <div class="section">
        <h2>📄 Upload Additional Information</h2>
        <p>Upload CISA FAQs, NASED guidelines, or other factual election information.</p>
        
        <div class="form-group">
            <label for="upload-title">Content Title:</label>
            <input type="text" id="upload-title" placeholder="e.g., CISA Election Security FAQ">
        </div>
        
        <div class="form-group">
            <label for="upload-type">Content Type:</label>
            <select id="upload-type">
                <option value="additional_info">Additional Information</option>
                <option value="myth">New Myth/Rumor</option>
                <option value="article">Debunking Article</option>
            </select>
        </div>
        
        <div class="form-group">
            <label for="upload-content">Content:</label>
            <textarea id="upload-content" placeholder="Paste or type the content here..."></textarea>
        </div>
        
        <div class="form-group">
            <label for="upload-file">Or Upload File:</label>
            <input type="file" id="upload-file" accept=".txt,.md,.pdf">
        </div>
        
        <button onclick="uploadContent()">Upload Content</button>
    </div>

    <!-- Content Selection Section -->
    <div class="section">
        <h2>🎯 Select Misinformation & Articles</h2>
        
        <div class="form-group">
            <label for="myth-select">Choose Myth/Rumor to Debunk:</label>
            <select id="myth-select">
                <option value="">Loading myths...</option>
            </select>
        </div>
        
        <div class="form-group">
            <label for="article-select">Choose Debunking Article:</label>
            <select id="article-select">
                <option value="">Loading articles...</option>
            </select>
        </div>
        
        <div class="form-group">
            <label for="additional-info-select">Additional Information (optional):</label>
            <select id="additional-info-select" multiple>
                <option value="">Loading additional info...</option>
            </select>
            <small>Hold Ctrl/Cmd to select multiple items</small>
        </div>
        
        <button onclick="generateInoculation()">Generate Inoculation Dose</button>
    </div>

    <!-- Inoculation Output Section -->
    <div class="section">
        <h2>💉 Generated Inoculation Dose</h2>
        <div id="inoculation-output" class="inoculation-output">
            <p>Select content above and click "Generate Inoculation Dose" to create your personalized inoculation.</p>
        </div>
        
        <h3>💬 Test the Inoculation (Interactive Chat)</h3>
        <div id="chat-container" class="chat-container">
            <p><em>Generated inoculation will appear here for testing...</em></p>
        </div>
        
        <div class="form-group">
            <input type="text" id="user-input" placeholder="Type your message here..." disabled>
            <button id="send-button" onclick="sendMessage()" disabled>Send</button>
        </div>
    </div>

    <!-- Content Library Section -->
    <div class="section">
        <h2>📚 Content Library</h2>
        <div id="content-library" class="content-list">
            <p>Loading content library...</p>
        </div>
    </div>

    <script>
        let currentInoculationConfig = null;
        let chatHistory = [];

        // Load initial content
        window.onload = function() {
            loadContentLibrary();
            loadContentSelectors();
        };

        async function uploadContent() {
            const title = document.getElementById('upload-title').value;
            const type = document.getElementById('upload-type').value;
            const content = document.getElementById('upload-content').value;
            const fileInput = document.getElementById('upload-file');
            
            if (!title || (!content && !fileInput.files[0])) {
                alert('Please provide a title and content');
                return;
            }

            const formData = new FormData();
            formData.append('title', title);
            formData.append('content_type', type);
            
            if (fileInput.files[0]) {
                formData.append('file', fileInput.files[0]);
            } else {
                formData.append('content', content);
            }

            try {
                const response = await fetch('/api/content/upload', {
                    method: 'POST',
                    body: formData
                });
                
                if (response.ok) {
                    alert('Content uploaded successfully!');
                    loadContentLibrary();
                    loadContentSelectors();
                    // Clear form
                    document.getElementById('upload-title').value = '';
                    document.getElementById('upload-content').value = '';
                    fileInput.value = '';
                } else {
                    alert('Upload failed: ' + await response.text());
                }
            } catch (error) {
                alert('Upload error: ' + error.message);
            }
        }

        async function loadContentLibrary() {
            try {
                const response = await fetch('/api/content/list');
                const content = await response.json();
                
                const library = document.getElementById('content-library');
                library.innerHTML = '';
                
                Object.entries(content).forEach(([type, items]) => {
                    const section = document.createElement('div');
                    section.innerHTML = `
                        <h3>${type.charAt(0).toUpperCase() + type.slice(1)}</h3>
                        ${items.map(item => `
                            <div class="content-item">
                                <h4>${item.title}</h4>
                                <p><strong>Type:</strong> ${item.content_type}</p>
                                <p><strong>Is False Claim:</strong> ${item.is_false_claim ? 'Yes' : 'No'}</p>
                                <p>${item.content.substring(0, 200)}...</p>
                            </div>
                        `).join('')}
                    `;
                    library.appendChild(section);
                });
            } catch (error) {
                console.error('Failed to load content library:', error);
            }
        }

        async function loadContentSelectors() {
            try {
                // Load myths
                const mythsResponse = await fetch('/api/content/myth');
                const myths = await mythsResponse.json();
                const mythSelect = document.getElementById('myth-select');
                mythSelect.innerHTML = '<option value="">Select a myth...</option>';
                myths.forEach(myth => {
                    mythSelect.innerHTML += `<option value="${myth.id}">${myth.title}</option>`;
                });

                // Load articles
                const articlesResponse = await fetch('/api/content/article');
                const articles = await articlesResponse.json();
                const articleSelect = document.getElementById('article-select');
                articleSelect.innerHTML = '<option value="">Select an article...</option>';
                articles.forEach(article => {
                    articleSelect.innerHTML += `<option value="${article.id}">${article.title}</option>`;
                });

                // Load additional info
                const infoResponse = await fetch('/api/content/additional_info');
                const info = await infoResponse.json();
                const infoSelect = document.getElementById('additional-info-select');
                infoSelect.innerHTML = '';
                info.forEach(item => {
                    infoSelect.innerHTML += `<option value="${item.id}">${item.title}</option>`;
                });
            } catch (error) {
                console.error('Failed to load content selectors:', error);
            }
        }

        async function generateInoculation() {
            const mythId = document.getElementById('myth-select').value;
            const articleId = document.getElementById('article-select').value;
            const additionalInfoIds = Array.from(document.getElementById('additional-info-select').selectedOptions)
                .map(option => option.value);

            if (!mythId || !articleId) {
                alert('Please select both a myth and an article');
                return;
            }

            try {
                const response = await fetch('/api/inoculation/generate', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        myth_id: mythId,
                        article_id: articleId,
                        additional_info_ids: additionalInfoIds
                    })
                });

                if (response.ok) {
                    const result = await response.json();
                    currentInoculationConfig = result;
                    
                    const output = document.getElementById('inoculation-output');
                    output.innerHTML = `
                        <h3>✅ Inoculation Dose Generated</h3>
                        <p><strong>Myth:</strong> ${result.myth.title}</p>
                        <p><strong>Article:</strong> ${result.article.title}</p>
                        <p><strong>Additional Info:</strong> ${result.additional_info.map(info => info.title).join(', ') || 'None'}</p>
                        <details>
                            <summary>View System Prompt</summary>
                            <pre style="white-space: pre-wrap; background: #f0f0f0; padding: 10px; margin: 10px 0;">${result.system_prompt}</pre>
                        </details>
                    `;
                    
                    // Enable chat
                    document.getElementById('user-input').disabled = false;
                    document.getElementById('send-button').disabled = false;
                    document.getElementById('chat-container').innerHTML = '<p><em>Chat initialized. Start a conversation to test the inoculation!</em></p>';
                    chatHistory = [];
                    
                } else {
                    alert('Generation failed: ' + await response.text());
                }
            } catch (error) {
                alert('Generation error: ' + error.message);
            }
        }

        async function sendMessage() {
            const input = document.getElementById('user-input');
            const message = input.value.trim();
            
            if (!message || !currentInoculationConfig) return;
            
            // Add user message to chat
            const chatContainer = document.getElementById('chat-container');
            chatContainer.innerHTML += `<div class="message user-message"><strong>You:</strong> ${message}</div>`;
            
            input.value = '';
            chatHistory.push({role: 'user', content: message});
            
            try {
                const response = await fetch('/api/inoculation/chat', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        inoculation_config: currentInoculationConfig,
                        message: message,
                        chat_history: chatHistory
                    })
                });
                
                if (response.ok) {
                    const result = await response.json();
                    chatContainer.innerHTML += `<div class="message assistant-message"><strong>Assistant:</strong> ${result.response}</div>`;
                    chatHistory.push({role: 'assistant', content: result.response});
                    chatContainer.scrollTop = chatContainer.scrollHeight;
                } else {
                    chatContainer.innerHTML += `<div class="message assistant-message"><strong>Error:</strong> ${await response.text()}</div>`;
                }
            } catch (error) {
                chatContainer.innerHTML += `<div class="message assistant-message"><strong>Error:</strong> ${error.message}</div>`;
            }
        }

        // Allow Enter key to send messages
        document.addEventListener('DOMContentLoaded', function() {
            document.getElementById('user-input').addEventListener('keypress', function(e) {
                if (e.key === 'Enter') {
                    sendMessage();
                }
            });
        });
    </script>
</body>
</html>
        """
        return web.Response(text=html_content, content_type='text/html')
    
    async def list_content(self, request: Request) -> Response:
        """List all content organized by type"""
        content_by_type = {}
        
        for content_type in ['myth', 'article', 'additional_info', 'placebo']:
            content_list = self.config_manager.list_content_by_type(content_type)
            content_by_type[content_type] = [
                {
                    'id': content.id,
                    'title': content.title,
                    'content': content.content,
                    'content_type': content.content_type,
                    'is_false_claim': content.is_false_claim
                }
                for content in content_list
            ]
        
        return json_response(content_by_type)
    
    async def get_content_by_type(self, request: Request) -> Response:
        """Get content of a specific type"""
        content_type = request.match_info['content_type']
        content_list = self.config_manager.list_content_by_type(content_type)
        
        result = [
            {
                'id': content.id,
                'title': content.title,
                'content': content.content,
                'content_type': content.content_type,
                'is_false_claim': content.is_false_claim
            }
            for content in content_list
        ]
        
        return json_response(result)
    
    async def upload_content(self, request: Request) -> Response:
        """Upload new content"""
        try:
            data = await request.post()
            title = data.get('title')
            content_type = data.get('content_type')
            content_text = data.get('content', '')
            
            # Handle file upload
            if 'file' in data:
                file_field = data['file']
                if hasattr(file_field, 'file'):
                    content_text = (await file_field.read()).decode('utf-8')
            
            if not title or not content_text or not content_type:
                return web.Response(text="Missing required fields", status=400)
            
            # Create content piece
            content_id = f"{content_type}_{uuid.uuid4().hex[:8]}"
            content_piece = ContentPiece(
                id=content_id,
                title=title,
                content=content_text,
                content_type=content_type,
                is_false_claim=(content_type == 'myth')
            )
            
            self.config_manager.add_content(content_piece)
            
            return json_response({
                'success': True,
                'content_id': content_id,
                'message': 'Content uploaded successfully'
            })
            
        except Exception as e:
            return web.Response(text=str(e), status=500)
    
    async def generate_inoculation(self, request: Request) -> Response:
        """Generate an inoculation configuration"""
        try:
            data = await request.json()
            myth_id = data.get('myth_id')
            article_id = data.get('article_id')
            additional_info_ids = data.get('additional_info_ids', [])
            
            config = self.config_manager.create_inoculation_config(
                myth_id=myth_id,
                article_id=article_id,
                additional_info_ids=additional_info_ids
            )
            
            system_prompt = self.config_manager.generate_system_prompt(config)
            
            return json_response({
                'myth': {
                    'id': config.myth.id,
                    'title': config.myth.title,
                    'content': config.myth.content
                },
                'article': {
                    'id': config.article.id,
                    'title': config.article.title,
                    'content': config.article.content
                },
                'additional_info': [
                    {
                        'id': info.id,
                        'title': info.title,
                        'content': info.content
                    }
                    for info in config.additional_info
                ],
                'system_prompt': system_prompt
            })
            
        except Exception as e:
            return web.Response(text=str(e), status=500)
    
    async def chat_with_inoculation(self, request: Request) -> Response:
        """Handle chat interaction with inoculation"""
        try:
            data = await request.json()
            message = data.get('message')
            chat_history = data.get('chat_history', [])
            inoculation_config = data.get('inoculation_config')
            
            # Build full message history
            messages = [
                {'role': 'system', 'content': inoculation_config['system_prompt']}
            ]
            messages.extend(chat_history)
            messages.append({'role': 'user', 'content': message})
            
            # Generate response
            response_text = ""
            async for chunk in my_generate(
                messages,
                model=self.config_manager.base_config['large_model'],
                use_custom_endpoint=self.config_manager.base_config['use_custom_endpoint'],
                max_tokens=self.config_manager.base_config['max_tokens'],
                stream=False
            ):
                response_text += chunk
            
            return json_response({
                'response': response_text.strip()
            })
            
        except Exception as e:
            return web.Response(text=str(e), status=500)
    
    async def export_config(self, request: Request) -> Response:
        """Export current configuration"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"electionbot_config_{timestamp}.json"
        
        config_data = {
            'exported_at': datetime.now().isoformat(),
            'base_config': self.config_manager.base_config,
            'content_pieces': {
                id: {
                    'id': content.id,
                    'title': content.title,
                    'content': content.content,
                    'content_type': content.content_type,
                    'is_false_claim': content.is_false_claim
                }
                for id, content in self.config_manager.content_pieces.items()
            }
        }
        
        return web.Response(
            text=json.dumps(config_data, indent=2),
            content_type='application/json',
            headers={'Content-Disposition': f'attachment; filename="{filename}"'}
        )
    
    async def import_config(self, request: Request) -> Response:
        """Import configuration from file"""
        try:
            data = await request.post()
            if 'file' not in data:
                return web.Response(text="No file provided", status=400)
            
            file_field = data['file']
            content = (await file_field.read()).decode('utf-8')
            config_data = json.loads(content)
            
            # Load content pieces
            for id, data in config_data.get('content_pieces', {}).items():
                content = ContentPiece(**data)
                self.config_manager.add_content(content)
            
            return json_response({'success': True, 'message': 'Configuration imported successfully'})
            
        except Exception as e:
            return web.Response(text=str(e), status=500)

def create_app() -> web.Application:
    """Create and configure the web application"""
    app = InoculationApp()
    return app.app

if __name__ == '__main__':
    app = create_app()
    config_manager = ConfigManager()
    
    host = config_manager.base_config['host']
    port = config_manager.base_config['internal_port']
    
    print(f"Starting Election Misinformation Inoculation Tool on {host}:{port}")
    web.run_app(app, host=host, port=port)