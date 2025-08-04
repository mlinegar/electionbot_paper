# This file imports from the main launch_chatserver for backwards compatibility
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from launch_chatserver import *