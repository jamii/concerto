Multiplayer clojure!

Concerto adds a broadcast mode to nrepl, so that when multiple users are logged into the same nrepl server they can each see what the others are doing.

For the time being, concerto is just an experiment and will not be supported. Known bugs include poor handling of multiple joins and broken stacktraces. I *suspect* the latter is due to broken decoding of nested dicts in [nrepl.el](https://github.com/kingtim/nrepl.el) - certainly the received messages lose fields as soon as concerto begins broadcasting.

# Usage

```clojure
[concerto "0.1.0-SNAPSHOT"]
```

On the server side:

```clojure
jamie@alien:~/concerto$ lein repl
...
user=> (require 'concerto)
nil
user=> (def server (concerto/server))
#'user/server
```

On the client side:

  * load [concerto.el](https://raw.github.com/jamii/concerto/master/concerto.el) in emacs

  * M-x nrepl [host] [port]

  * M-x concerto [username]

You should now get broadcast messages in the \*concerto\* buffer for every repl interaction.
