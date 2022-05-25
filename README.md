# NewsTyping

CLI English news typing application  
This application uses [News API](https://newsapi.org)

## Demo (x4 speed)

![demo](./docs/demo.gif)

In this demo, the following articles are used.

```json
{
  "articles": [
    {
      "source": { "id": null, "name": "Deadline" },
      "author": "Nellie Andreeva",
      "title": "‘This Is Us’: How NBC Drama Wrapped Pearsons’ Story In Moving Series Finale - Deadline",
      "description": "Six years ago today, the trailer for then-new NBC drama series This Is Us was blowing up, breaking records with about 80 million views in 12 days. It was a precursor to the show’s phenomenal six-season run which ended tonight with the series finale, titled “U…",
      "url": "https://deadline.com/2022/05/this-is-us-series-finale-recap-jack-rebecca-randall-kate-kevin-season-6-episode-18-1235031154/",
      "urlToImage": "https://deadline.com/wp-content/uploads/2022/05/NUP_197545_02415.jpg?w=1000",
      "publishedAt": "2022-05-25T02:01:00Z",
      "content": "Six years ago today, the trailer for then-new NBC drama seriesThis Is Us was blowing up, breaking records with about 80 million views in 12 days. It was a precursor to the show’s phenomenal six-seaso… [+6766 chars]"
    },
    {
      "source": { "id": null, "name": "SciTechDaily" },
      "author": null,
      "title": "Sharkcano! – NASA Satellite Catches Submarine Eruption of Kavachi Volcano - SciTechDaily",
      "description": "Kavachi Volcano in the Southwest Pacific Solomon Islands—where hammerhead sharks prowl—has entered an active phase of eruption. The Solomon Islands' Kavachi Volcano is one of the most active submarine volcanoes in the Pacific Ocean. According to the Smithsoni…",
      "url": "https://scitechdaily.com/?p=173470",
      "urlToImage": "https://scitechdaily.com/images/Eruption-of-Kavachi-Volcano-May-2022-Annotated.jpg",
      "publishedAt": "2022-05-24T22:08:01Z",
      "content": "BySara E. Pratt, NASA Earth ObservatoryMay 24, 2022May 14, 2022. A plume of discolored water being emitted by Kavachi Volcano.\r\nKavachi Volcano in the Southwest Pacific Solomon Islandswhere hammerhea… [+2494 chars]"
    }
  ]
}
```

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
