# visualize_graph.py
import asyncio
from electionbot_core.graph import ConversationGraph

def main():
    # Instantiate the graph
    conversation_graph = ConversationGraph()
    
    # Get the Mermaid representation
    mermaid_diagram = conversation_graph.graph.get_graph().draw_mermaid()
    
    print("--- MERMAID DIAGRAM ---")
    print(mermaid_diagram)
    print("--- END DIAGRAM ---")

if __name__ == "__main__":
    main()
