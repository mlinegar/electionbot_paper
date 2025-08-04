# This file imports from the main my_generate for backwards compatibility
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from my_generate import *