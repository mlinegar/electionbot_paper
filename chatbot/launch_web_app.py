#!/usr/bin/env python3
"""
Quick launcher for the Election Misinformation Inoculation Web App
"""
import asyncio
import sys
from base.web_app import create_app
from base.config_modular import ConfigManager
from aiohttp import web

def main():
    print("🗳️  Election Misinformation Inoculation Tool")
    print("=" * 50)
    
    # Create app
    app = create_app()
    config_manager = ConfigManager()
    
    host = config_manager.base_config['host']
    port = config_manager.base_config['internal_port']
    
    print(f"📍 Server starting on: http://{host}:{port}")
    print(f"📊 Database: {config_manager.base_config['db_name']}")
    print(f"🤖 AI Model: {config_manager.base_config['large_model']}")
    print(f"🔗 API Endpoint: {'Custom' if config_manager.base_config['use_custom_endpoint'] else 'OpenAI'}")
    print("-" * 50)
    print("📝 Available features:")
    print("  • Upload CISA FAQs, NASED guidelines, custom content")
    print("  • Select myths/rumors and debunking articles") 
    print("  • Generate personalized inoculation doses")
    print("  • Test inoculations with interactive chat")
    print("  • Export/import configurations")
    print("-" * 50)
    print("🚀 Starting server...")
    
    try:
        web.run_app(app, host=host, port=port)
    except KeyboardInterrupt:
        print("\n👋 Server stopped by user")
    except Exception as e:
        print(f"❌ Error starting server: {e}")
        sys.exit(1)

if __name__ == '__main__':
    main()