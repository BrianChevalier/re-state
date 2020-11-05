(ns app.index)

(def github-logo
  (str
   "<svg class=github-corner xmlns=\"http://www.w3.org/2000/svg\" width=\"80\" height=\"80\" viewBox=\"0 0 250 250\" fill=\"#151513\" style=\"position: absolute; top: 0; right: 0\">"
   "<path d=\"M0 0l115 115h15l12 27 108 108V0z\" fill=\"#fff\"/>"
   "<path class=\"octo-arm\" d=\"M128 109c-15-9-9-19-9-19 3-7 2-11 2-11-1-7 3-2 3-2 4 5 2 11 2 11-3 10 5 15 9 16\" style=\"-webkit-transform-origin: 130px 106px; transform-origin: 130px 106px\"/>"
   "<path class=\"octo-body\" d=\"M115 115s4 2 5 0l14-14c3-2 6-3 8-3-8-11-15-24 2-41 5-5 10-7 16-7 1-2 3-7 12-11 0 0 5 3 7 16 4 2 8 5 12 9s7 8 9 12c14 3 17 7 17 7-4 8-9 11-11 11 0 6-2 11-7 16-16 16-30 10-41 2 0 3-1 7-5 11l-12 11c-1 1 1 5 1 5z\"/>"
   "</svg>"))

(def logo-button
  (let [url "https://github.com/BrianChevalier/re-state"]
    (str
     "<style>.github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}</style>"
     "<a href=" url ">"
     github-logo
     "</a>")))

(defn html []
  (let [code-url "main.js"
        title "Dynamics Simulator"]
    (str
     "<!DOCTYPE html>"
     "<html>"
     "<head>"
     "<title>" title "</title>"
     "<meta charset='UTF-8' />"
     "<meta name='viewport' content='width=device-width, initial-scale=1' />"
     "<link href=\"https://fonts.googleapis.com/css?family=Lato|Roboto:300,400\" rel= \"stylesheet\" >"
     "<link href= \"https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@400&display=swap\" rel= \"stylesheet\" >"
     "<style>
      p {font-family: 'Roboto', sans-serif;}
      h1 {font-family: 'Roboto', sans-serif;}
      td {font-family: 'Roboto', sans-serif;
      height: 20px;}
      code {font-family: 'Roboto Mono', monospace;}
      body {margin: 0px; background-color: #2e3440;}
      input::-webkit-outer-spin-button,
      input::-webkit-inner-spin-button {
        display: none;
      }
      </style>"
     "</head>"
     "<body style=\"margin: 0\">"
     logo-button
     "<div id=\"root\"></div>"
     "<script src=\"" code-url "\"></script>"
     "</body>"
     "</html>")))

(defn -main []
  (println (html)))