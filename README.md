# NewsTyping
CLI English news typing application  
This application uses [News API](https://newsapi.org)

## Config

in config.yaml

```yaml:config.yaml
key: String
fixConsoleWidth: Bool
```

### key
your apikey for [News API](https://newsapi.org)

### fixConsoleWidth
if an unintended line break is output on an ANSI terminal, please enable it.

## How to use

### Build with stack

```bash
cd NewsTyping
stack build
cp ./config.yaml .stack-work/path/to/install/dir
stack run
```

### Use executable file
edit config.yaml then put it in the same dir as the executable file.  
Optionally, add the path to executable file to your environment variables.

## Run

```bash
newstyping
```

Start game with 3 articles.

```bash
newstyping 5
```

Start game with 5 articles.  
You can specify the number of articles.

```bash
newstyping help
```

Show help.
