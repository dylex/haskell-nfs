# Haskell NFS library.

The current focus is a working, user-space, NFSv4 client.

## Example

```haskell
import qualified Network.NFS.V4 as NFS

client <- NFS.openClient "nfs.server.example.com"
NFS.REMOVE4res'NFS4_OK (NFS.REMOVE4resok cinfo) <-
  NFS.nfsCall client $ NFS.opFileReference NFS.FileRoot *> NFS.op (NFS.REMOVE4args "delete.me")
```

## References

This package implements [RFC5661](https://tools.ietf.org/html/rfc5661), NFSv4.1.
The [XDR specification](Network/NFS/V4/Prot.x) was extracted and minimally edited from [RFC5662](https://tools.ietf.org/html/rfc5662).
