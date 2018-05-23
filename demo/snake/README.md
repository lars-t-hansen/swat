# Snake

This is a simple game that runs in a web browser and demonstrates the
use of Swat in a web page.

For now, you must have recent Firefox Nightly, and in about:config you
must set javascript.options.wasm_gc to `true`.

Then, run `serve` in the current directory to serve the files, and go
to http://localhost:8000/snake.html in the browser.

Or, if you have your own web server, ensure that .wasm files are
served as MIME type `application/wasm`, and copy snake.html, snake.js,
and snake.wasm to a directory on your server and load snake.html from
there.
