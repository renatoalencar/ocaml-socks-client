import socket
import codecs


def to_hex(b):
    return codecs.encode(b, 'hex')


class Socks4:

    def make_request_packet_ip(self, destip, destport, command):
        version = b'\x04'
        port = bytes([0xff & (destport >> 8), 0xff & destport])
        ip = socket.inet_aton(destip)

        return version + command + port + ip + b'\0'

    def make_request_packet_domain(self, domain, destport, command):
        req = self.make_request_packet_ip('0.0.0.1', destport, command)
        return req + bytes(domain, 'utf-8') + b'\0'

    def connect(self, s, dest):
        ip, port = dest

        try:
            request = self.make_request_packet_ip(ip, port, b'\1')
        except OSError:
            request = self.make_request_packet_domain(ip, port, b'\1')

        print('Sending ', to_hex(request))
        s.send(request)

        response = s.recv(8)
        print('Received ', to_hex(response))

        assert response[0] == 0

        if response[1] == '\90':
            print('Request granted')
        elif response[1] == '\91':
            print('Request rejected or failed')
        elif response[1] == '\92':
            printf('Socks cannot connect to identd')
        elif response[1] == '\93':
            printf('User id doesnt match')

connection = socket.socket()
connection.connect(('127.0.0.1', 9050))
socks = Socks4()

domain = 'torchdeedp3i2jigzjdmfpn5ttjhthh5wbmda2rr3jvqjg5p77c54dqd.onion'

socks.connect(connection, (domain, 80))

connection.send(f'''GET /search?query=socks4 HTTP/1.1\r
Host: {domain}\r
User-Agent: curl/7.79.1\r
Accept: */*\r
\r
'''.encode('utf-8'))
response = connection.recv(4096)

print(response.decode('utf-8'))
