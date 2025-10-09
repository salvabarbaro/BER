import os
import pandas as pd
from dotenv import load_dotenv
from tqdm import tqdm
import time
import openai

## Requirements for replication:
## You need an openai-key (token) 
## The process will be charged by openai (about 15 USD)

# load the api key (in ".env"")
load_dotenv(dotenv_path=".env")
api_key = os.getenv("OPENAI_API_KEY")
client = openai.OpenAI(api_key=api_key)

# Load a csv file with the statements
df = pd.read_csv("Statements23.csv")

# Prompting (in German, because the statements are in German, too)
def build_prompt(statement):
    return f"""
Bitte bewerte das folgende politische Statement auf einer Skala von 1 bis 4 im Hinblick auf Föderalismus:

**Skala:**
1 = betont die Eigenständigkeit des eigenen Bundeslandes und/oder betont die eigenen, selbständigen Entscheidungen
2 = betont gute Recht, von bundeseinheitlichen Entscheidungen abzuweichen. Stellt sich nicht grundsätzlich gegen bundesweite Entscheidungen  
3 = gemeinsame Entscheidungen von Bund und Ländern werden befürwortet / Abweichungen anderer Länder werden kritisiert  
4 = fordert einheitliche Regelungen oder kritisiert zu wenig zentrale Maßnahmen

**Beispiele:**

**Bewertung 1:**  
"Wir haben uns deshalb hier mit breiter Mehrheit auf unseren MV-Weg verständigt. Mecklenburg-Vorpommern ist in einen strengen Lockdown gegangen, teilweise über das hinaus, was der Bund für Deutschland festgelegt hatte."

**Bewertung 2:**  
"Eine Maskenpflicht für ganz Deutschland sehe ich derzeit nicht", sagt Ministerpräsident Armin Laschet im Unterschied zu Markus Söder.

**Bewertung 3:**  
„Ich bin froh, dass wir uns heute gemeinsam als Länder und Bund auf den Weg gemacht haben“, sagt MP @ArminLaschet.

**Bewertung 4:**  
"Ich fordere vom Bund nun schnelles Handeln mit bundeseinheitlichen Standards."

---

**Zu bewerten:**  
"{statement}"

Gib bitte nur eine Zahl von 1 bis 4 zurück.
"""

# Classfication
def classify_statement(statement, retry_delay=5):
    prompt = build_prompt(statement)
    try:
        response = client.chat.completions.create(
            model="gpt-4",  # or "gpt-4o" or other LLM model version
            messages=[
                {"role": "user", "content": prompt}
            ],
            temperature=0
        )
        return response.choices[0].message.content.strip()
    except Exception as e:
        print(f"Error: {e}. Try again in {retry_delay} seconds...")
        time.sleep(retry_delay)
        return classify_statement(statement, retry_delay + 2)

# Main Loop
results = []
for text in tqdm(df["Text"], desc="Bewerte Statements"):
    score = classify_statement(text)
    results.append(score)

# Store results in a CSV file
df["Bewertung"] = results
df.to_csv("Statements_bewertet.csv", index=False)

print("Classification accomplished and file 'Statements_bewertet.csv' has been generated")
