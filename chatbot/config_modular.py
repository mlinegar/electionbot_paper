#!/usr/bin/env python3
"""
Modular configuration system for ElectionBot
Supports dynamic content loading and user customization
"""
import json
import os
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from pathlib import Path
from dotenv import load_dotenv

@dataclass
class ContentPiece:
    """Represents a piece of content (myth, article, etc.)"""
    id: str
    title: str
    content: str
    content_type: str  # 'myth', 'article', 'additional_info'
    is_false_claim: bool = False  # True for myths, False for facts
    
@dataclass
class InoculationConfig:
    """Configuration for generating inoculation doses"""
    myth: ContentPiece
    article: ContentPiece
    additional_info: List[ContentPiece]
    system_prompt_template: str
    
class ConfigManager:
    """Manages modular configuration for ElectionBot"""
    
    def __init__(self, env_file: str = None):
        # Load environment
        env_file = env_file or os.getenv('ENV_FILE', '/home/mlinegar/electionbot/.env')
        load_dotenv(env_file)
        
        # Base configuration
        self.base_config = self._load_base_config()
        
        # Content storage
        self.content_pieces: Dict[str, ContentPiece] = {}
        self.content_categories = {
            'myths': [],
            'articles': [], 
            'additional_info': [],
            'placebo': []
        }
        
        # Load default content
        self._load_default_content()
    
    def _load_base_config(self) -> Dict[str, Any]:
        """Load base system configuration"""
        return {
            # Database settings
            'db_name': os.getenv('DB_NAME'),
            'db_user': os.getenv('DB_USER'), 
            'db_password': os.getenv('DB_PASSWORD'),
            'db_host': os.getenv('DB_HOST', 'localhost'),
            'db_port': os.getenv('DB_PORT'),
            
            # API settings
            'openai_api_key': os.getenv('OPENAI_API_KEY'),
            'openai_organization': os.getenv('OPENAI_ORGANIZATION'),
            'openai_api_url': os.getenv('OPENAI_API_URL'),
            'custom_api_url': os.getenv('CUSTOM_API_URL'),
            'token_url': os.getenv('TOKEN_URL'),
            'client_id': os.getenv('CLIENT_ID'),
            'client_secret': os.getenv('CLIENT_SECRET'),
            'scope': os.getenv('SCOPE'),
            
            # Model settings
            'use_custom_endpoint': False,  # Default to OpenAI
            'small_model': 'gpt-4o-mini',
            'large_model': 'gpt-4o',
            'max_tokens': 1500,
            
            # Server settings
            'host': '127.0.0.1',
            'internal_port': 8081,
            'external_port': 8080,
            'script_name': 'election2024_v2',
            'max_depth': 2
        }
    
    def _load_default_content(self):
        """Load default content from current config.py"""
        
        # Default myths
        myth1 = ContentPiece(
            id="myth1_noncitizen",
            title="Non-citizen Voting", 
            content="Non-citizens were able to register and vote in large numbers, at levels significant enough to impact election outcomes in certain areas.",
            content_type="myth",
            is_false_claim=True
        )
        
        myth2 = ContentPiece(
            id="myth2_voter_rolls",
            title="Inaccurate Voter Rolls",
            content="Voter registration systems and rolls were highly inaccurate or easily manipulated, containing many ineligible voters whose votes may have been illegally counted.",
            content_type="myth", 
            is_false_claim=True
        )
        
        placebo = ContentPiece(
            id="placebo_voting_plan",
            title="Voting Preparation",
            content="I have the information I need to vote in the 2024 election, and am aware of some of the potential difficulties in doing so.",
            content_type="placebo",
            is_false_claim=False
        )
        
        # Default articles (simplified for brevity)
        myth1_article1 = ContentPiece(
            id="myth1_article1",
            title="Non-citizen Voting Debunk - Arizona",
            content="""THESE CLAIMS ARE FALSE.
            
During election seasons, you may encounter various claims about the integrity of voting systems. It's crucial to approach such information critically and seek reliable sources for verification.

Recently, a conservative group called America First Legal filed a lawsuit against Maricopa County, Arizona, alleging that non-citizens are illegally registered to vote in significant numbers...

[Article content continues with factual rebuttals]""",
            content_type="article",
            is_false_claim=False
        )
        
        # Add to storage
        for content in [myth1, myth2, placebo, myth1_article1]:
            self.add_content(content)
    
    def add_content(self, content: ContentPiece):
        """Add a content piece to the manager"""
        self.content_pieces[content.id] = content
        
        # Categorize
        if content.content_type == 'myth':
            self.content_categories['myths'].append(content.id)
        elif content.content_type == 'article':
            self.content_categories['articles'].append(content.id)
        elif content.content_type == 'additional_info':
            self.content_categories['additional_info'].append(content.id)
        elif content.content_type == 'placebo':
            self.content_categories['placebo'].append(content.id)
    
    def get_content(self, content_id: str) -> Optional[ContentPiece]:
        """Get content by ID"""
        return self.content_pieces.get(content_id)
    
    def list_content_by_type(self, content_type: str) -> List[ContentPiece]:
        """Get all content of a specific type"""
        ids = self.content_categories.get(content_type, [])
        return [self.content_pieces[id] for id in ids if id in self.content_pieces]
    
    def upload_content_from_file(self, file_path: str, content_id: str, title: str, content_type: str):
        """Upload content from a file"""
        with open(file_path, 'r') as f:
            content = f.read()
        
        piece = ContentPiece(
            id=content_id,
            title=title,
            content=content,
            content_type=content_type,
            is_false_claim=(content_type == 'myth')
        )
        
        self.add_content(piece)
        return piece
    
    def create_inoculation_config(self, myth_id: str, article_id: str, 
                                additional_info_ids: List[str] = None) -> InoculationConfig:
        """Create configuration for generating inoculation doses"""
        
        myth = self.get_content(myth_id)
        article = self.get_content(article_id)
        
        if not myth or not article:
            raise ValueError(f"Missing required content: myth={myth_id}, article={article_id}")
        
        # Get additional info
        additional_info = []
        if additional_info_ids:
            for info_id in additional_info_ids:
                info = self.get_content(info_id)
                if info:
                    additional_info.append(info)
        
        # Default system prompt template
        system_prompt_template = """
You are an AI academic chatbot designed to inoculate voters against election misinformation. Your role is to engage in conversations with users, understand their beliefs about election integrity, and provide factual information to counter potential misinformation.

You are discussing the following election myth: {MYTH}

You will use this article both to inform your responses and to present the myth to the user for discussion: {ARTICLE}

Additional information to remember: {ADDITIONAL_INFO}

[Rest of prompt continues...]
"""
        
        return InoculationConfig(
            myth=myth,
            article=article, 
            additional_info=additional_info,
            system_prompt_template=system_prompt_template
        )
    
    def generate_system_prompt(self, config: InoculationConfig) -> str:
        """Generate the system prompt from configuration"""
        additional_info_text = "\n\n".join([info.content for info in config.additional_info])
        
        return config.system_prompt_template.format(
            MYTH=config.myth.content,
            ARTICLE=config.article.content,
            ADDITIONAL_INFO=additional_info_text
        )
    
    def save_config_to_file(self, file_path: str):
        """Save current configuration to a JSON file"""
        config_data = {
            'base_config': self.base_config,
            'content_pieces': {
                id: {
                    'id': content.id,
                    'title': content.title,
                    'content': content.content,
                    'content_type': content.content_type,
                    'is_false_claim': content.is_false_claim
                }
                for id, content in self.content_pieces.items()
            }
        }
        
        with open(file_path, 'w') as f:
            json.dump(config_data, f, indent=2)
    
    def load_config_from_file(self, file_path: str):
        """Load configuration from a JSON file"""
        with open(file_path, 'r') as f:
            config_data = json.load(f)
        
        # Update base config
        self.base_config.update(config_data.get('base_config', {}))
        
        # Load content pieces
        for id, data in config_data.get('content_pieces', {}).items():
            content = ContentPiece(**data)
            self.add_content(content)

# Global instance
config_manager = ConfigManager()

def get_config_manager() -> ConfigManager:
    """Get the global configuration manager"""
    return config_manager