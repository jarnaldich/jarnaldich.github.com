---
title: Dealing with CORS in JupyterLite
date: 2023-01-31T00:00:00
tags: jupyterlite, CORS, data, data, webassembly
---

Following my [previous
post](blog/2022/12/08/data-manipulation-jupyterlite.html), I am intending to see
how far I can push JupyterLite as a platform for data analysis in the browser.
The convenience of having a full enviroment with a sensible default set of
libraries for dealing with data [one link
away](https://jupyterlite.github.io/demo/lab/index.html) is really something I could use.

But of course, for data analysis you need... well... data. There is certainly no
shortage of public datasets on the internet, many of them falling into some sort
of Open Data initiatives, such as the [EU Open
Data](https://data.europa.eu/en/publications/open-data-maturity/2022).

But, as soon as you try to use JupyterLite to directly fetch data from those
sites, you find yourself stumping on a wall named CORS.

## Cross Origin Request Policy

The Cross Origin Request Policiy (CORS) is a protection system designed to prevent a
family of attacks known as Cross Site Scripting or XSS. Browsers implement this
protection by not allowing a page to perform requests to a server that is
different from where it wast downloaded unless this other server explicitly
allows for it.

This behaviour bites hard at any application involving third party data
analysis in the browser, as well as a lot of webassembly "ports" of existing
applications with networking capabilities, since the original desktop apps
were not designed to deal with this kind of restrictions[^webvm].

![&nbsp;](/images/cors.png "CORS"){.center}

For example, if you are using the Jupyterlite at `jupyterlite.github.io`, you
will not be able to fetch any server beyond `github.io` that does not allow for
it specifically... which many data providers don't. The request will be blocked
by the browser itself (step 2 in the diagram above). You will either need to
download yourself the data and upload it to JupyterLite, or self-host
JupyterLite and the data in your own server (using it as a proxy for data
requests), which kinda takes all the convenience out of it. As an example,
evaluating this snippet in JupyterLite works exactly as you would expect:

```python
import pandas as pd
from js import fetch

WORKS = "https://raw.githubusercontent.com/jupyterlite/jupyterlite/main/examples/data/iris.csv"
WORKS_CORS_ENABLED  = "https://data.wa.gov/api/views/f6w7-q2d2/rows.csv?accessType=DOWNLOAD"
FAILS_CORS_DISABLED = "https://opendata-ajuntament.barcelona.cat/data/dataset/1121f3e2-bfb1-4dc4-9f39-1c5d1d72cba1/resource/69ae574f-adfc-4660-8f81-73103de169ff/download/2018_menors.csv"

res = await fetch(WORKS)
text = await res.text()
print(text)
```

There are two ways in which a data provider can accept cross-origin requests. The
main one (the "canonical" one) is by adding explicit permission in the HTTP
headers. Whenever this is not possible or practical (it needs access to the HTTP
server configuration, and some hosting providers may not allow it), there is a
second way: the JSONP callback.

## The JSONP Callback 

The JSONP callback works along these lines:

1. The calling page (eg. JupyterLite) defines a callback function, with a data parameter.
2. The calling page (JupyterLite) loads a script from the data provider, passing
   a the name of the callback function.
3. The data provider script calls the callback function with the requested data.
   
Since the script was downloaded from the data provider's domain, it can perform
requests to that domain, so CORS restrictions do not apply. 

This is not the recommended solution because it delegates to the application
something that belongs to another layer: both the server and the consuming
webpage have to modified. One typical use case is making older browsers work.
The other is kind of accidental: downloading from (poorly configured?) Open Data
portals. Most Open Data portals (including administrative ones) use pre-built
data management systems such as [CKAN](https://ckan.org). These often can
handle JSONP by default, while http servers have CORS disabled by default. So
keeping the defaults leaves you with JSONP.

## Implementing a JSONP helper in JupyterLite

One of the things I love about the browser as a platform is that it is... pretty
hackable... just press F12 and you can enter the kitchen. For example, you can see
how JupyterLite "fakes" its filesystem on top of
[IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API),
wich is an API for storing persistent data in the browser.

So, we have a way to perform CORS requests and get data from a server
implementing JSONP, and we can also fiddle with JupyterLite's virtual
filesystem... would it be possible to write a helper to download datasets into
the virtual filesystem? You bet! Just paste the following code in a javascript
kernel cell, or use the `%%javascript` magic in a python one:

```javascript
window.saveJSONP = async (urlString, file_path, mime_type='text/json', binary=false) => {
    const sc = document.createElement('script');
    var url = new URL(urlString);
    url.searchParams.append('callback', 'window.corsCallBack');
    
    sc.src = url.toString();

    window.corsCallBack = async (data) => {
        console.log(data);

        // Open (or create) the file storage
        var open = indexedDB.open('JupyterLite Storage');

        // Create the schema
        open.onupgradeneeded = function() {
            throw Error('Error opening IndexedDB. Should not ever need to upgrade JupyterLite Storage Schema');
        };

        open.onsuccess = function() {
            // Start a new transaction
            var db = open.result;
            var tx = db.transaction("files", "readwrite");
            var store = tx.objectStore("files");

            var now = new Date();

            var value = {
                'name': file_path.split(/[\\/]/).pop(),
                'path': file_path,
                'format': binary ? 'binary' : 'text',
                'created': now.toISOString(),
                'last_modified': now.toISOString(),
                'content': JSON.stringify(data),
                'mimetype': mime_type,
                'type': 'file',
                'writable': true
            };      

            const countRequest = store.count(file_path);
            countRequest.onsuccess = () => {
              console.log(countRequest.result);
                if(countRequest.result > 0) {
                    store.put(value, file_path);
                } else {
                    store.add(value, file_path);
                }   
            }; 

            // Close the db when the transaction is done
            tx.oncomplete = function() {
                db.close();
            };
        }
    }

    document.getElementsByTagName('head')[0].appendChild(sc);
}
```

Then, each time you need to download a file, you can just use the following javascript:

```javascript
%%javascript
var url = 'https://opendata-ajuntament.barcelona.cat/data/es/api/3/action/datastore_search?resource_id=69ae574f-adfc-4660-8f81-73103de169ff'
window.saveJSONP(url, 'data/menors.json')
```

To clarify, you should either use a python kernel with the `%%javascript` magic
or the javascript kernel in *both* the definition and the call, otherwise they
won't see each other.

Then from a python cell we can read it the standard way:

```python
import json
import pandas as pd

with open('data/menors.json', 'r') as f:
  data = json.load(f)
  
pd.read_json(json.dumps(data['result']['records']))
```

## Conclusions

- We are just starting to see the potential of WebAssembly based solutions and the
  browser environment (IndexedDB...).

- If you are a data provider, you should seriously consider enabling CORS to
  promote the usage of your data. Otherwise you will be banning a growing market of
  web-based analysis tools from your data.


## References

- Simple IndexedDB [example](https://gist.github.com/JamesMessinger/a0d6389a5d0e3a24814b)
- [Sample code](https://github.com/jupyterlite/jupyterlite/discussions/91?sort=new) for
  reading and writing files in JupyterLite (this is where the idea for this post
  comes from).
- [On CORS](https://enable-cors.org/) and how to enable it. 
- A test [web page](https://www.test-cors.org/) to check if a server is CORS enabled.
- Some data providers with CORS enabled:
    - <https://catalog.data.gov/dataset/?res_format=CSV>
 
[^webvm]: If you are curious about the possible solutions to this problems, you
    may like to read how [WebVM](https://webvm.io/), a server-less virtual
    Debian, implements a general solution 
    [here](https://leaningtech.com/webvm-virtual-machine-with-networking-via-tailscale/).
