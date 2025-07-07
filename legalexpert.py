import streamlit as st
from dashscope import Generation
import dashscope
from pyswip import Prolog
import os
import re
import graphviz
import uuid

# === CONFIGURATION ===
DASHSCOPE_API_KEY = 'sk-0cb128a3e6d14c2eb467498b3de8705f'
kb_path = os.path.expanduser("~/Desktop/knowledge base.pl")

# === DASHSCOPE SETUP ===
dashscope.base_http_api_url = 'https://dashscope-intl.aliyuncs.com/api/v1' 

# === PROLOG KB SETUP ===
prolog = Prolog()
prolog.consult(kb_path)

# === UTILITY: Extract Prolog Rules from Comments ===
def extract_prolog_rules(file_path):
    with open(file_path, 'r') as file:
        code = file.read()
    pattern = r'%\s*(.*?)\n([a-z_][a-zA-Z0-9_]*\(.*?\))\s*:-'
    matches = re.findall(pattern, code, re.DOTALL)
    return [{"description": desc.strip(), "rule": rule.strip()} for desc, rule in matches]

rules_summary = extract_prolog_rules(kb_path)

# === LLM FUNCTION ===
def ask_llm(messages, rules_summary):
    rules_text = "\n".join([f"- {r['rule']}: {r['description']}" for r in rules_summary])
    system_prompt = (
        "You are a legal expert. Your only source of knowledge is the Prolog rules in knowledge base.pl.\n"
        "Do not use any external knowledge or generate information beyond these rules.\n"
        "If no relevant rule is found, respond appropriately by informing the user that you cannot answer the question due to lack of relevant information in the current knowledge base.\n"
        "The knowledge base pertains to the Non-Muslim Family Law System in Malaysia, based on the Law Reform (Marriage and Divorce) Act 1976.\n"
        "Always assume all users are non-Muslim and that their cases fall under the Law Reform (Marriage and Divorce) Act 1976. No further confirmation of these points is required.\n"
        "Remember all prior user responses and context. Never state that the user hasn't responded if they already have.\n"
        "When a user asks a question:\n"
        "Match it with the relevant Prolog rule(s).\n"
        "Ask follow-up question(s) only if necessary information is missing.\n"
        "Keep follow-up questions short and only one at a time.\n"
        "Do not provide a final conclusion until all required facts are confirmed.\n"
        "Once the conclusion is certain, provide a concise final answer.\n"
        "Every time and only after delivering the final answer (regardless of the answer's content), include:\n"
        "The matched Prolog rule(s) in bold\n"
        "A visual decision tree diagram showing how the answer was derived\n"
        "\nProlog Rules:\n" + rules_text
    )
    full_messages = [{'role': 'system', 'content': system_prompt}] + messages
    response = Generation.call(
        api_key=DASHSCOPE_API_KEY,
        model="qwen-plus",
        messages=full_messages,
        result_format='message'
    )
    return response.output.choices[0].message.content.strip()

# === PARSE DECISION TREE FOR VISUALIZATION ===
def parse_decision_tree(answer_text):
    lines = [line.strip() for line in answer_text.split('\n') if "if" in line.lower() and "then" in line.lower()]
    graph = graphviz.Digraph(comment="Decision Tree")
    graph.attr(rankdir='TB', size='8,5')
    node_ids = {}

    for i, line in enumerate(lines):
        match = re.match(r'(?i).*if\s+(.*?)\s*,?\s*then\s+(.*?)[\.\n]?', line)
        if match:
            condition, outcome = match.groups()
            cond_id = node_ids.get(condition) or str(uuid.uuid4())
            outc_id = node_ids.get(outcome) or str(uuid.uuid4())
            node_ids[condition] = cond_id
            node_ids[outcome] = outc_id
            graph.node(cond_id, condition)
            graph.node(outc_id, outcome)
            graph.edge(cond_id, outc_id)
    return graph if lines else None

# === SESSION STATE INITIALIZATION ===
if 'history' not in st.session_state:
    st.session_state.history = []
if 'admin_logged_in' not in st.session_state:
    st.session_state.admin_logged_in = False
if 'admin_visible' not in st.session_state:
    st.session_state.admin_visible = False

# === UI SETUP ===
st.set_page_config(page_title="Legal Expert System", layout="centered")
st.markdown("<h1 style='text-align: center;'>⚖️ Legal Expert System</h1>", unsafe_allow_html=True)
st.markdown("<p style='text-align: center; color: gray;'>AI-powered reasoning based on the Non-Muslim Family Law System of Malaysia</p>", unsafe_allow_html=True)

# === ADMIN PANEL - HIDDEN BY DEFAULT ===
# Toggle admin panel visibility
if st.sidebar.button("🔑 Admin"):
    st.session_state.admin_visible = not st.session_state.admin_visible

# Only show admin panel if toggled
if st.session_state.admin_visible:
    st.sidebar.markdown("🔐 **Admin Login**")
    admin_username = st.sidebar.text_input("Username", type="default", key="admin_user")
    admin_password = st.sidebar.text_input("Password", type="password", key="admin_pass")
    login_button = st.sidebar.button("Login", key="login_btn")

    ADMIN_USERNAME = "admin"
    ADMIN_PASSWORD = "SecurePassw0rd!2025"

    if login_button:
        if admin_username == ADMIN_USERNAME and admin_password == ADMIN_PASSWORD:
            st.session_state.admin_logged_in = True
            st.sidebar.success("Logged in as Admin")
        else:
            st.sidebar.error("Invalid credentials")
            st.session_state.admin_logged_in = False

    # Show edit section if logged in
    if getattr(st.session_state, "admin_logged_in", False):
        st.sidebar.markdown("## 🛠️ Edit Knowledge Base")
        try:
            with open(kb_path, 'r') as f:
                kb_content = f.read()
            new_kb_content = st.sidebar.text_area("Edit Prolog Rules:", value=kb_content, height=400, key="kb_editor")

            if st.sidebar.button("Save Changes", key="save_kb"):
                with open(kb_path, 'w') as f:
                    f.write(new_kb_content)
                # Reload Prolog
                prolog = Prolog()
                prolog.consult(kb_path)
                # Refresh rule summary
                rules_summary = extract_prolog_rules(kb_path)
                st.sidebar.success("Knowledge base saved and reloaded.")
                st.rerun()

        except Exception as e:
            st.sidebar.error(f"Error reading knowledge base: {str(e)}")

# === MAIN CHAT INTERFACE ===
with st.container():
    st.markdown("#### 📝 Ask a Legal Question")
    st.info("I utilize a Prolog knowledge base governed by the Law Reform (Marriage and Divorce) Act 1976.")
    user_input = st.text_input("Enter your legal question here:", placeholder="E.g., Can I apply for custody if...")

    col1, col2 = st.columns([1, 4])
    with col1:
        submitted = st.button("Submit")

    if submitted:
        if not user_input.strip():
            st.warning("Please enter a question.")
        else:
            with st.spinner("Analyzing your input..."):
                try:
                    st.session_state.history.append({'role': 'user', 'content': user_input})
                    answer = ask_llm(st.session_state.history, rules_summary)
                    st.session_state.history.append({'role': 'assistant', 'content': answer})

                    st.markdown("### 🤖 Answer")
                    if answer.lower().startswith("sorry"):
                        st.info(answer)
                    else:
                        st.success(answer)

                        # Visualize dynamic decision tree
                        tree_graph = parse_decision_tree(answer)
                        if tree_graph:
                            st.markdown("#### 🧩 Reasoning Process (Decision Tree):")
                            st.graphviz_chart(tree_graph)
                except Exception as e:
                    st.error(f"An error occurred: {str(e)}")
