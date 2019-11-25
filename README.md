# cl-enet

Common lisp wrapper around [Enet](http://enet.bespin.org/) using [cl-autowrap](https://github.com/rpav/cl-autowrap)

cl-enet currently supports a minimal set of features from the enet
library. See example.lisp for a demonstration where a server and
client send a number back and forth to each other, each time adding a
random value between 0 and 4 to the number. To test the example run
the following:

```
(enet-examples:start-server)
(enet-examples:start-client)
```

You will want to run `(force-output)` occasionally to see new output.
Running force-output inside the threads was causing issues with slime.

There's currently no documentation. The entirety of the library's
functions are implemented inside enet.lisp.

