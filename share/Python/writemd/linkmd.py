from markdown.preprocessors import Preprocessor
from markdown.extensions import Extension
import re


mylink_re = re.compile("""\[([^]]+)\]\((?!.+://)([^)]+)\.md(#.*)?\)""")
codeblock_re = re.compile("""^    """)

class MdLinkPreprocessor(Preprocessor):
    def run(self, lines):
        new_lines = []
        for line in lines:
            ms = mylink_re.finditer(line)
            offset = 0
            oldline = line
            newline = ""
            match_detected = False
            for m in ms:
                match_detected = True
                (b,e) = m.span()
                newline = newline + line[0:(b - offset)] + \
                          "[" + m.group(1) + "](" + \
                          m.group(2) + ".html" + (m.group(3) if m.group(3) else "") + ")"
                oldline = oldline[e:]
                offset  = offset + e
            new_lines.append(newline if ((not codeblock_re.match(line)) and match_detected) else line)
        return new_lines

class MdLinkExtension(Extension):
    def extendMarkdown(self, md, md_globals):
        md.preprocessors.add('mdlinks', MdLinkPreprocessor(md), '_begin')
