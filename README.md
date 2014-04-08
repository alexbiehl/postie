postie
======

http://hackage.haskell.org/package/postie

`postie` is a little smtp server library for receiving emails. It is currently
in a very early stage and not yet fully standard compatible although the standard
protocol is already supported.
.
* At the time of writing parameters on smtp commands can not be parsed.
.
* Handler functions need to return more detailed information to cancel a transaction.
.
`postie` supportes hooks on key commands in an smtp session like
.
* on connection open and close
.
* on /MAIL FROM/ command e.g. to intercept transaction if sender is blacklisted
.
* on /RCPT TO/ command e.g. to check if recipient is existent on your server
.
* and many more


To run `postie` you only need to supply a function which takes a `Mail` and
return `Accepted` or `Rejected`. `mailBody` is a `pipes` `Producer` which
streams the encoded body directly to your application code. The body is not
parsed by `postie` since it depends on what the application wants to do with
the mail data. Eventually I will create a seperate package for parsing mime
messages with `pipes-parse` when postie becomes more stable and standard compliant.
