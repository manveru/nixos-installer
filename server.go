package main

import (
	"encoding/json"
	"fmt"
	"html"
	"html/template"
	"log"
	"net/http"
	"os"
	"os/exec"
	"regexp"
	"sync"
	"time"
)

// Router routes all the requests
type Router struct {
	routes  []routeMap
	writer  http.ResponseWriter
	request *http.Request
}

var compileMutex = sync.RWMutex{}

func main() {
	var routes = []routeMap{}

	route := func(pattern string, action func(Router, string), needsLock bool) {
		routes = append(routes, routeMap{
			pattern:   regexp.MustCompile(pattern),
			action:    action,
			needsLock: needsLock,
		})
	}

	route("^/timezones/?$", Router.serveTimezones, false)
	route("^/disks/?$", Router.serveDisks, false)
	route("^/options/?$", Router.serveOptions, false)
	route("^/assets/(.+)$", Router.serveAssets, false)
	route("^/_compile/(.+\\.elm)$", Router.serveElmAsJS, true)
	route("^/(.*)$", Router.serveIndex, false)

	server := &http.Server{
		Addr:           ":8080",
		Handler:        &handler{routes: routes},
		ReadTimeout:    10 * time.Second,
		WriteTimeout:   10 * time.Second,
		MaxHeaderBytes: 1 << 20,
	}
	log.Fatal(server.ListenAndServe())
}

func (r Router) serveIndex(path string) {
	r.serveFile("ui/index.html")
}

func (r Router) serveAssets(path string) {
	r.serveFile("ui/assets/" + path)
}

// Unfortunately, `elm-make` is not very customizable. so we need to do things
// in the `ui` directory where the `elm-stuff` lives. And we have to write to an
// actual file, because it relies on the filename extension to decide whether to
// render HTML or Javascript. So we wrap this function into a mutex that
// everything else has to wait for. // In production we'll not compile on every
// request, so this won't be needed.
func (r Router) serveElmAsJS(path string) {
	fmt.Println(path)
	err := os.Chdir("ui")
	if err != nil {
		log.Fatal(err)
	}
	defer (func() {
		err := os.Chdir("..")
		if err != nil {
			log.Fatal(err)
		}
	})()

	cmd := exec.Command("elm-make", "--debug", "--yes", "Main.elm", "--output", "index.js")
	out, err := cmd.CombinedOutput()
	if err != nil {
		r.writeError(err.Error(), string(out))
	} else {
		r.serveFile("index.js")
	}
}

const listNixOptions = `
with builtins;
let
	lib = (import <nixpkgs> {}).lib;
	options = (import <nixpkgs/nixos> {}).options;
	optionsList = lib.filter (opt: opt.visible && !opt.internal) (lib.optionAttrSetToDocList options);
	substFunction = x:
		if isAttrs x then lib.mapAttrs (name: substFunction) x
		else if isList x then map substFunction x
		else if isFunction x then "<function>"
		else x;
	optionsList' = lib.flip map optionsList (opt: opt // {
		declarations = map stripAnyPrefixes opt.declarations;
	}
	// lib.optionalAttrs (opt ? example) { example = substFunction opt.example; }
	// lib.optionalAttrs (opt ? default) { default = substFunction opt.default; }
	// lib.optionalAttrs (opt ? type) { type = substFunction opt.type; });
	prefixesToStrip = map (p: "${toString p}/") ([ ../../.. ]);
	stripAnyPrefixes = lib.flip (lib.fold lib.removePrefix) prefixesToStrip;
in
	listToAttrs (map (o: {
		name = o.name;
		value = removeAttrs o ["name" "visible" "internal"];
	}) optionsList')
`

func (r Router) serveOptions(path string) {
	cmd := exec.Command("nix-instantiate", "--json", "--strict", "--eval", "-E", listNixOptions)
	out, err := cmd.CombinedOutput()
	if err != nil {
		r.writeJSON(map[string]string{
			"code":  err.Error(),
			"error": string(out),
		})
	} else {
		r.writeRawJSON(out)
	}
}

func (r Router) serveNixosManual(path string) {
	root := os.Getenv("NIXOS_MANUAL")
	r.serveFile(root + path)
}

func (r Router) serveTimezones(_ string) {
	r.writeJSON(ParseTimezones())
}

func (r Router) serveDisks(_ string) {
	r.writeJSON(ParseDisks())
}

func (r Router) route() {
	path := r.request.URL.Path
	for _, route := range r.routes {
		matches := route.pattern.FindStringSubmatch(path)
		// fmt.Println(route.pattern, path, matches)
		if len(matches) > 0 {
			route.callAction(r, matches[len(matches)-1])
			return
		}
	}

	fmt.Fprintf(r.writer, "Hello, %q", html.EscapeString(path))
}

func (r Router) serveFile(path string) {
	fmt.Println(path)
	http.ServeFile(r.writer, r.request, path)
}

func (r Router) writeJSON(data interface{}) {
	enc := json.NewEncoder(r.writer)
	enc.SetIndent("", "  ")

	r.writer.Header().Set("Content-Type", "application/json")
	r.writer.WriteHeader(200)
	err := enc.Encode(data)
	if err != nil {
		log.Fatal(err)
	}
}

func (r Router) writeRawJSON(data []byte) {
	r.writer.Header().Set("Content-Type", "application/json")
	r.writer.WriteHeader(200)
	_, err := r.writer.Write(data)
	if err != nil {
		log.Fatal(err)
	}
}

func (r Router) writeError(header, body string) {
	r.writer.Header().Set("Content-Type", "text/javascript")
	r.writer.WriteHeader(200)
	innerHTML := template.JSEscapeString(
		`
<!DOCTYPE html>
<html>
  <head>
    <title>` + template.HTMLEscapeString(header) + `</title>
    <style>
body {
  background: #111;
  color: #eee;
  display: flex;
  margin: 0;
  padding: 0;
  align-items: center;
  min-height: 50vh;
  justify-content: center;
}

pre {
  padding: 1em;
  background: #222;
}
		</style>
  </head>
  <body><pre>` + template.HTMLEscapeString(body) + `</pre></body>
</html>
`)

	code := `setTimeout(function(){ document.body.innerHTML = "` + innerHTML + `" }, 1)`

	_, err := r.writer.Write([]byte(code))
	if err != nil {
		log.Fatal(err)
	}
}

type handler struct {
	routes []routeMap
}

func (h *handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	router := Router{routes: h.routes, writer: w, request: r}
	router.route()
}

type routeMap struct {
	pattern   *regexp.Regexp
	action    func(Router, string)
	needsLock bool
}

func (rm routeMap) callAction(r Router, match string) {
	if rm.needsLock {
		compileMutex.Lock()
		defer (func() { compileMutex.Unlock() })()
		rm.action(r, match)
	} else {
		compileMutex.RLock()
		defer (func() { compileMutex.RUnlock() })()
		rm.action(r, match)
	}
}
