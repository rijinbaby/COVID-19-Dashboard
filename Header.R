header <- dashboardHeader(title = "COVID-19 ITALY",
                          disable = FALSE,
                          titleWidth = 270
                          ,dropdownMenuCustom( type = 'message',
                                              customSentence = customSentence,
                                              messageItem(
                                                from = "ceeds@unimi.it",#'Feedback and suggestions',
                                                message =  "",#paste0("ceeds@unimi.it" ),
                                                icon = icon("envelope"),
                                                href = "mailto:ceeds@unimi.it"
                                              ),
                                              icon = icon('comment')
                          )
                          ,dropdownMenuCustom( type = 'message',
                                              customSentence = customSentence_share,
                                              icon = icon("share-alt"),
                                              messageItem(
                                                from = 'Twitter',
                                                message = "",
                                                icon = icon("twitter"),
                                                href = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fceeds.unimi.it%2Fcovid-19-in-italy%2F&text=COVID-Pro%3A%20a%20province-based%20analysis%20for%20Italy"
                                              ),
                                              messageItem(
                                                from = 'LinkedIn',
                                                message = "",
                                                icon = icon("linkedin"),
                                                href = "http://www.linkedin.com/shareArticle?mini=true&url=https%3A%2F%2Fceeds.unimi.it%2Fcovid-19-in-italy%2F&title=COVID-Pro%3A%20a%20province-based%20analysis%20for%20Italy"
                                              ),
                                              messageItem(
                                                from = 'Facebook',
                                                message = "",
                                                icon = icon("facebook"),
                                                href = "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fceeds.unimi.it%2Fcovid-19-in-italy%2F"
                                              )
                                              
                          )
                          )





























