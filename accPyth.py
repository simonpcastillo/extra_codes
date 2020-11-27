import re

pattern = re.compile(r"annottations\.txt")
links = [a["href"] for a in soup.find_all("a", href=pattern)]