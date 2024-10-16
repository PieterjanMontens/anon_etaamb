[![WARNING](https://img.shields.io/badge/Warning-Legacy-orange.svg)](https://shields.io/)
# Simple, basic dictionnary based anonymization component
*This is legacy code*

This component is used to anonymize the content from the Belgian Official Journal, as used by [etaamb.be](https://www.etaamb.be) since 2010
(though etaamb added multiple pattern-based checks, which this component does not provide).

It has been slightly patched to improve stability and usage in a modern docker environment, but most of it has remained unchanged for the past 10 years, and is considered **legacy** and **extremely perfectible**.

It is now meant to be used as a simple demonstrator, and as what can be achieved by simple, naive yet efficient means.


## Usage
This was above all an early learning project, the first I built in erlang (and as such does did not include OTP or any other modern toolset), and communicated with PHP through a C module.
Code is inefficient, and reinvents standard available erlang functionality. It runs on Erlang/OTP 17, and was written under version R12B (yes, it's that old, JSON wasn't even a standard back then).

The `start.sh` scripts run both the `store` and `anoner` components.

#### Store
Storage module, containing a dictionnary of words. On startup, it loads a backup of both french en dutch words, ready
for usage by the anonymisation component.

#### Anoner
Can be interrogated by word or by word list. Returns a "confidence" score for each word:

* 100 for regular words
* 0 for non-regulard words

##### Functions:
* `anoner:check/2` : checks a single word 
* `anoner:multi_check/2` : checks a string, changes words for score in-place
* `anoner:sequence_check/2`: a more efficient string tokenizer, changes words for score in-place

Recommended usage is: provide space-separted strings of words, and get score back based on word position, using the `sequence_check/2/ function.

## 2020 Updates
### Added a web server (mochiweb) for querying via HTTP
Usage:
- GET / `check` , provide `word` and `lang` (dutch or french)
- POST / `sequence_check`, provide `string` and `lang` (dutch or french)

Mochiweb must be cloned and build in the `server/deps` directory (support verified for OTP version 22)
```bash
$ git clone --depth=1 https://github.com/mochi/mochiweb.git server/deps/mochiweb
$ cd server/deps/mochiweb
$ rebar co
```

Examples:
```bash
$ curl --header "Content-Type: application/json"  --request POST  --data '{"string":"poulet jambon pour Jules crème", "lang":"french"}' http://localhost:8000/sequence_check
> 100 100 100 0 100
$ curl -v "http://localhost:8000/check?lang=french&word=immeuble"
> 100
```

### Run with docker
Using docker erlang:22-slim
```bash
$ docker build -t "anon" ./  && docker run --rm -it -p8050:8050 anon
```
Same curl commands as above can be run to test the service.
