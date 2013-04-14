iprange
=======

Tools for expanding / compressing ip addresses

Examples
========

Expand 192.168.1.0/24
---------------------

```
# ipexpand -cidr 26 192.168.1.0/24
192.168.1.192/26
192.168.1.128/26
192.168.1.64/26
192.168.1.0/26
```

```
# ipexpand -netmask 255.255.255.192 192.168.1.0/24
192.168.1.192/26
192.168.1.128/26
192.168.1.64/26
192.168.1.0/26
```


Compress 192.168.1.0/24 - 192.168.7.0/24
----------------------------------------

```
# ./ipcompress 192.168.1.0-192.168.7.255
192.168.7.0/32
192.168.6.0/24
192.168.4.0/23
192.168.2.0/23
192.168.1.0/24
```

```
# cat > ipblocks.txt
192.168.1.0/24
192.168.2.0/24
192.168.3.0/24
192.168.4.0/24
192.168.5.0/24
192.168.6.0/24
192.168.7.0/24

# cat ipblocks.txt | ./ipcompress
192.168.4.0/22
192.168.2.0/23
192.168.1.0/24
```

Output in different format
--------------------------

```
# ./ipcompress -type netmask 192.168.1.0-192.168.7.255
192.168.4.0/255.255.252.0
192.168.2.0/255.255.254.0
192.168.1.0/255.255.255.0
```

```
# ./ipcompress -type wildcard 192.168.1.0-192.168.7.255
```
192.168.4.0/0.0.3.255
192.168.2.0/0.0.1.255
192.168.1.0/0.0.0.255
```
