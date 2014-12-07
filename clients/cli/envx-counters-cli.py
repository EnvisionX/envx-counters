#!/usr/bin/env python

"""
Command line tool for reading counter values from
running instances of the envx_counters Erlang application.
"""

import os.path
import socket
import sys
import time


DEFAULTS = {
    'host' : 'localhost',
    'port' : 8907,
    'proto' : 'tcp',
    'pipe' : False,
    'cmd_args' : [],
    'read_timeout' : 1,  # socket read timeout, in seconds
    'update_period' : 1,  # one second (for pipe mode)
    'verbose' : False  # undocumented feature
    }
READ_BUF_SIZE = 10240


def usage():
    """
    Show the usage message and exit.
    """
    cmd = os.path.basename(sys.argv[0])
    sys.stdout.write(
        'Usage:\n'
        '\t%s -h | --help        show this memo;\n'
        '\t%s [options] list     list available counters;\n'
        '\t%s [options] get Counter1 [Counter2 [Counter3 [...]]]\n'
        '                        get current values for counters;\n'
        '\t%s [options] dump     get current values for all counters.\n'
        'Options:\n'
        '\t--host Hostname       set hostname. Default is localhost;\n'
        '\t--port PortNumber     set port number. Default is 8907;\n'
        '\t--udp                 use UDP instead of TCP;\n'
        '\t--pipe                only for \'get\' command.\n'
        '\t                      Continously read and print the counter\n'
        '\t                      values to the stdout.\n' %
        (cmd, cmd, cmd, cmd))
    sys.exit(1)


def parse_args(args):
    """
    Parse command line arguments.

    :param args: command line arguments.
    :type args: list of strings
    :rtype: dict
    """
    result = DEFAULTS
    while len(args) > 0:
        if args[0] == '--verbose':
            result['verbose'] = True
            args = args[1:]
            continue
        if args[0] == '--pipe':
            result['pipe'] = True
            args = args[1:]
            continue
        if args[0] == '--host':
            result['host'] = args[1]
            args = args[2:]
            continue
        elif args[0].startswith('--host='):
            result['host'] = args[0][7:]
            args = args[1:]
            continue
        if args[0] == '--port':
            result['port'] = int(args[1])
            args = args[2:]
            continue
        elif args[0].startswith('--port='):
            result['port'] = int(args[0][7:])
            args = args[1:]
            continue
        if args[0] == '--udp':
            result['proto'] = 'udp'
            args = args[1:]
            continue
        if args[0].startswith('-'):
            sys.stderr.write(
                '%s: unknown option: %s\n' % (sys.argv[0], args[0]))
            sys.exit(1)
        if args[0] not in ('list', 'get', 'dump'):
            sys.stderr.write(
                '%s: unknown command: %s\n' % (sys.argv[0], args[0]))
            sys.exit(1)
        result['cmd'] = args[0]
        args = args[1:]
        if result['cmd'] == 'list' and len(args) > 0:
            sys.stderr.write(
                '%s: extra args after \'list\' command: %r\n' %
                (sys.argv[0], args))
            sys.exit(1)
        if result['cmd'] == 'dump' and len(args) > 0:
            sys.stderr.write(
                '%s: extra args after \'dump\' command: %r\n' %
                (sys.argv[0], args))
            sys.exit(1)
        result['cmd_args'] = args
        args = []
    if 'cmd' not in result:
        usage()
    if result['pipe']:
        if result['cmd'] != 'get':
            sys.stderr.write(
                '%s: pipe mode allowed only for \'get\' command\n' %
                (sys.argv[0],))
            sys.exit(1)
    return result


def do_udp(args, socket_type = socket.SOCK_DGRAM):
    """
    Do request over a UDP/IP proto.

    :param args: parsed command line arguments
    :type args: dict
    :param socket_type: socket type
    :type socket_type: socket.SOCK_DGRAM | socket.SOCK_STREAM
    """
    sock = socket.socket(type = socket_type)
    sock.settimeout(args['read_timeout'])
    sock.connect((args['host'], args['port']))
    cmd = args['cmd']
    if cmd == 'list':
        sock.send('%s\n' % (cmd,))
        for name in sock.recv(READ_BUF_SIZE).split():
            sys.stdout.write(name + '\n')
    elif cmd == 'dump':
        sock.send('list\n')
        for counter in sock.recv(READ_BUF_SIZE).split():
            sock.send('get %s\n' % counter)
            _, value = sock.recv(READ_BUF_SIZE).split()
            sys.stdout.write('%s %s\n' % (counter, value))
    else:
        while True:
            for name in args['cmd_args']:
                sock.send('%s %s\n' % (cmd, name))
                name, value = sock.recv(READ_BUF_SIZE).split()
                sys.stdout.write('%s %s\n' % (name, value))
            if not args['pipe']:
                break
            time.sleep(args['update_period'])
    sock.close()


def do_tcp(args):
    """
    Do request over a TCP/IP proto.

    :param args: parsed command line arguments
    :type args: dict
    """
    do_udp(args, socket.SOCK_STREAM)


def main():
    """
    Entry point
    """
    args = sys.argv[1:]
    if '-h' in args or '--help' in args:
        usage()
    args = parse_args(args)
    if args['verbose']:
        sys.stdout.write('Parsed args: %r\n' % (args,))
    if args['proto'] == 'udp':
        do_udp(args)
    else:
        do_tcp(args)


if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        # interrupted by ^C
        sys.exit(127)
