from pyepc.server import EPCServer
from pyepc.client import EPCClient
from pyclang import sublimeclang, sublime

server = EPCServer(('localhost', 0))
scaa = sublimeclang.SublimeClangAutoComplete()
client = EPCClient()

def send_compilation_results(content):
    client.call_sync('log', [content])

@server.register_function
def init_client(port):
    client.connect(('localhost', port))

@server.register_function
def query_completions(filename, position, prefix, flags, tmp_file=None):
    print filename, position, prefix, flags
    view = sublime.View(filename, position, flags, tmp_file)
    return scaa.on_query_completions(view, prefix, [position])

@server.register_function
def warmup_cache(filename, position, prefix, flags, tmp_file=None):
    view = sublime.View(filename, position, flags, tmp_file)
    scaa.warmup_cache(view)

@server.register_function
def recompile(filename, position, prefix, flags, tmp_file=None):
    view = sublime.View(filename, position, flags, tmp_file)
    scaa.recompile(view, lambda: sublimeclang.display_compilation_results(view, send_compilation_results))

server.print_port()
server.serve_forever()
