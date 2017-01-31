\documentclass[a4paper,12pt]{article}

%include polycode.fmt

\usepackage[utf8]{inputenc}
\usepackage[margin=1in]{geometry}
\usepackage[parfill]{parskip}
\usepackage{hyperref}

\hypersetup{
    colorlinks=true,
    linkcolor=blue,
}

\title{\textbf{Haskell VNC Client}:\\
    Source Code Documentation}
\author{
    Brandon Byskov\\
    1068517\\
    \texttt{byskovbm@@mcmaster.ca}
    \and
    Xuchao Ding\\
    1233855\\
    \texttt{dingx3@@mcmaster.ca}
    \and
    Natalie Perna\\
    1066785\\
    \texttt{pernanm@@mcmaster.ca}
}
\date{April 19th, 2015}

\begin{document}

\maketitle
\newpage

\tableofcontents
\newpage

%include app/Main.lhs
%include src/Client.lhs
%include src/Client/Handshake.lhs
%include src/Client/Messages.lhs
%include src/Client/Network.lhs
%include src/Client/Security.lhs
%include src/Client/Types.lhs
%include src/Client/Window.lhs
%include src/Client/Window/Graphics.lhs
%include src/Client/Window/Input.lhs

\end{document}
