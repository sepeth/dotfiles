import os
import atexit
import readline
import rlcompleter

history_file = os.path.expanduser('~/.pythonhist')

def save_history(history_file=history_file):
    import readline
    readline.write_history_file(history_file)

if os.path.exists(history_file):
    readline.read_history_file(history_file)

atexit.register(save_history)
del atexit, readline, rlcompleter, save_history, history_file
