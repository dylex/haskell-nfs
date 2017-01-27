# Haskell NFS library.

The current focus is a working, user-space, NFSv4 client.

## Example

```haskell
import qualified Network.NFS.V4 as NFS

client <- NFS.openClient "nfs.server.example.com"
NFS.REMOVE4res (NFS.REMOVE4res'resok (NFS.REMOVE4resok cinfo)) <-
  NFS.nfsCall client (NFS.REMOVE4args "delete.me" <* NFS.opFileReference NFS.FileRoot)
```

## References

This package implements [RFC5661](https://tools.ietf.org/html/rfc5661), NFSv4.1.
The [XDR specification](Network/NFS/V4/Prot.x) was extracted and minimally edited from [RFC5662](https://tools.ietf.org/html/rfc5662).
