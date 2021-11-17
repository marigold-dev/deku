#!/usr/bin/env python3

# code quality: medium 2021-04-20

# pip3 install GitPython
from git import Repo
import re, subprocess, sys
from datetime import datetime, date

repo = Repo()

files = subprocess.check_output(['git', 'ls-tree', '-r', 'HEAD', '--name-only', '-z'])
files = files.decode('utf-8').strip('\0').split('\0')

scores = []

now = date.today()

for i,file in enumerate(files):
    print('\033[2K[' + str(round(i * 100 / len(files), 2)) + '% ' + file + ']', file=sys.stderr, end='\r')

    # rules on files to skip entirely go here:
    if file.startswith(('changelog/', 'gitlab-pages/', 'tools/webide/')):
        continue # skip file.
    
    commits = subprocess.check_output(['git', 'log', '--format=%H', file]).decode('utf-8').strip('\n').split('\n')

    # number of commits since last review
    # sum of length of diffs
    # length of file
    # for commit in commits:

    date_file_added = subprocess.check_output(['git', 'log', '--diff-filter=A', '--date=short', '--format=%ad', '--', file]).decode('utf-8').strip('\n').split('\n')[0]
    date_file_added = datetime.strptime(date_file_added, '%Y-%m-%d')
    days_since_file_added = (now - date_file_added.date()).days

    # default to last_refactor
    days_since_last_refactor = None
    head_contents = subprocess.check_output(['git', 'show', 'HEAD' + ':' + file])
    m = re.search(br'(last refactor: |code quality: (low|medium|good|high) )([0-9]{4}-[0-9]{2}-[0-9]{2})', head_contents)
    if m is not None:
        last = m.group(3).decode('utf-8')
        last = datetime.strptime(last, '%Y-%m-%d')
        days_since_last_refactor = (now - last.date()).days
    
    quality = 2
    m = re.search(br'code quality: (low|medium|good|high)', head_contents)
    if m is not None:
        q = m.group(1)
        if q == b'low':
            quality = 1
        elif q == b'medium':
            quality = 2
        elif q == b'good' or q == b'high':
            quality = 3
    
    #contents = subprocess.check_output(['git', 'show', commit + ':' + file]).strip(b'\n').split(b'\n')
    
    # one code quality point is worth 100 days of age (can be adjusted as needed)
    days = 1
    score_quality = (quality * 100 * days)
    score_age = (days_since_last_refactor or days_since_file_added)
    score = score_age + score_quality

    print('\033[2K', file=sys.stderr, end='')
    scores.append([file, score, 'age(days)', score_age, 'quality', quality])

scores.sort(key=lambda x: x[1])

print('\033[2K', file=sys.stderr, end='')
sys.stderr.flush()
print('\n'.join([x[0] + (','.join(str(info) for info in x[1:])) for x in scores]))
