"""
Convert old octopress include_code blocks to stuff in the actual code
"""

import glob
import re
import os

POSTS_PATH = 'content/post/'
CODE_PATH = os.getenv('HOME') + '/src/dantswain_old/source/downloads/code/'

for path in glob.glob(POSTS_PATH + '/*.md'):
    lines_out = []
    need_write = False
    with open(path, 'r') as post:
        for line in post:
            m = re.match(r'{% include_code .* (?P<code_path>[^ ]*) %}', line)
            if m:
                need_write = True
                with open(CODE_PATH + m.group('code_path'), 'r') as code_file:
                    code = code_file.read()

                lines_out.append('```elixir\n')
                lines_out.append(code)
                lines_out.append('```\n')
            else:
                lines_out.append(line)

    if need_write:
        with open(path, 'w') as new_post:
            new_post.write(''.join(lines_out))
