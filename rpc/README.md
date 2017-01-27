# Haskell ONC (Sun) RPC and XDR tools and library

Haskell tools and library for interacting with ONC RPC (aka Sun RPC) protocols.
The goal is a working NFS4 client, so TCP clients and efficient large data
handling will be prioritized over servers, UDP, and portmaping.  It includes
the following complete components.

## Primary Components

### hsrpcgen

A code-generator for .x (XDR) files which you can call directly or use as a
pre-processor from cabal (see [Setup.hs](Setup.hs) for an example).  The module
[Network.ONCRPC.Prot](Network/ONCRPC/Prot.hs) in this package is itself
generated from the [XDR description](Network/ONCRPC/Prot.x) of the RPC
protocol.  While many .x files will work as-is, some depend on C-specific
functionality that will require some minor tweaking.

### Network.ONCRPC.Client

A client library which allows you to use the generated module to make RPC calls
to servers.

## Example

```
hsrpcgen -P nfs.x -o NFS.hs
```

```haskell
import qualified Network.ONCRPC as RPC
import qualified NFS

client <- RPC.openClient $ RPC.ClientServerPort "nfs.server.example.com" "nfs"
reply <- RPC.rpcCall client $ RPC.Call (NFS.nFSPROC_NULL (NFS.nFS_VERSION NFS.nFS_PROGRAM)) RPC.AuthNone RPC.AuthNone ()
RPC.closeClient client
```

## References

This package implements [RFC5531](https://tools.ietf.org/html/rfc5531) and
[RFC4506](https://tools.ietf.org/html/rfc4506) as closely as possible.

[xdrgen](http://github.com/jthornber/xdrgen) is a rpcgen replacement for
other languages, written in Haskell.  By contrast, this project targets full RPC
support in Haskell.
