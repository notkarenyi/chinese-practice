# Chinese Practice

The main product in this project is `app.R`, an RShiny app that displays semantic links between Chinese words in a given vocabulary list. 

The interesting thing about Chinese is that its character structure uniquely lends it to root-word analysis, without the need for lemmatizing (at least, in the Chinese form). This will be obvious to native Chinese, but less so for Americans and heritage speakers like me.

## Resources for network graphing

[Katya Ognyanova: Network Visualization](https://kateto.net/network-visualization)

[Max Woolf: plotly in R](https://minimaxir.com/notebooks/interactive-network/)

[Francois Briatte: ggnetwork](https://briatte.github.io/ggnetwork/articles/ggnetwork.html)

[Andy Chen: Coupled Events with Shiny](https://medium.com/@abchen/increasing-interaction-through-coupled-events-with-shiny-and-plotly-4a253dd3be12)

## Prompt

1. I need to learn Chinese.
2. I have an existing list of vocab words to learn.
3. Duolingo is not advanced enough. I've completed almost every lesson.
4. Quizlet is not complex enough. I need to use the words in context, not just know what they mean.
5. I need unpredictable sentences. I can't translate the same few sentences. I would like some randomly generated prompts.
6. I need an app where I can organize the words in a 2D space and add tags/labels to make semantic connections
7. I need to be able to see common root words, whether computationally or by drawing connections

## Initial Plan

*wishlist

### Medium

* React app?
* RShiny?

### Sentence practice (Duolingo style) 

**2/2023: All of these capabilities are much better accomplished using ChatGPT lol. Pausing development**

* How to query my own .csv files? SQL?

* English sentence prompt

  * What kind of sentence templates to use?

    * write my own English templates?

    * get someone else's list of sentences to practice in other languages? where to find this?

    * draw verbatim from a children's book?

  * How to populate the template?

    * randomly mad-lib from the English column of my Chinese vocab list?
      * involves labeling my own list for parts of speech either by hand or using ML*
    * get someone else's list of common words and parts of speech?
    * try lemmatization and look at others' generators

* User input answer box and submit button

* Some kind of reaction, then next button

  * What kind of grading to implement?
    * no grading, just vibes
    * grading based on .csv lookup
    * grading based on Google translate* (would need API or something)
    * grading based on ML*

### Flashcard sorting

* Login (user input) to store user-specific information in separate .csvs*
* read in csv (using what package?)
* maybe highlight common root words and connect them automatically*

  * tokenize (split on character), then count and sort by frequency
  * select characters above a percentage or absolute threshold of frequency (or both/either) - variable common is TRUE
  * (in this vein, could implement pre-sorting before manual organization allowed)
  * when click preview, you can show/hide connections between root words - should we label the meaning?
    * connection is curved line drawn automatically between positions associated with the cards where common is TRUE (what is the math behind this)
    * color/connections grouped by root word (separate column after identifying common roots)
* card class (could maybe bootstrap this)

  * properties: rectangle, word (front), pinyin + definition (back), x, y, boolean selected, tag list (or single value), sub-tag, boolean flipped, boolean common, root word list (or single value)
  * when click, deselect others and select this one
  * when touching selection rectangle, boolean selected is TRUE
  * when click and drag, move
  * auto-sort button to arrange by tag (including untagged)
    * need to figure out the math behind this - something like number of cells in the matrix determined by the number of tags total, the x and y position determined by number of cells and a counter in a loop

  * study mode
    * when click, flip
    * when ctrl + click, hide (?)
  * organize mode
    * when selected is TRUE and add tag button clicked or shift + T, add tag in tag column of data frame (user input box)
* save option to save data frame to csv (with username*)

