# Prolog Web Server for AST Generation

Prolog web server which returns a JSON AST representation for a given Prolog source code snippet.

Using the `server.pl` it is possible to create an HTTP server listening for POST requests with Prolog source code snippets as `text/plain` and returning the appropriate AST as JSON.

The HTTP server can be started using the following command, which creates a local HTTP server on port 8081:

```bash
swipl server/server.pl --port=8081
```

The project's Makefile also provides a `make server` command.

### Message Protocol

The server has a single endpoint: `/`. It can be used only for POST requests. The Prolog source code is expected as the message body with `text/plain` as Content-Type. It will return the AST as `application/json` as response.

For example, the AST for the program `and(1,X,X)` can be generated using the following `curl` command from command line:

```bash
curl -H "Accept: text/x-prolog" -H "Content-Type: plain/text" -X POST -d 'and(1,X,X).' http://localhost:8081/
```

By providing the HTTP header `Accept: application/json`, the JSON representation needed for ASTExplorer is created:

```bash
curl -H "Accept: application/json" -H "Content-Type: plain/text" -X POST -d 'and(1,X,X).' http://localhost:8081/
```

If the given Prolog source code is not syntactically correct at all, the following JSON gets replied:

```json
{
  "type": "error",
  "message": "Program could not be parsed."
}
```
