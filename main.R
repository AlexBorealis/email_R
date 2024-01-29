## Setting working directory ----
setwd(getwd())

## Loading need packages ----
source(paste0(getwd(), "/need_pckgs.R"), local = T)

## Reading need variables ----
creds <- data.table(read.table('mail_vars.txt', header = T))
tg <- data.table(read.table('bot_vars.txt', header = T))
chat <- data.table(read.table('tg_vars.txt', header = T))
host <- data.table(read.table('host_vars.txt', header = T))

## Creation connection to mail ----
con <- configure_imap(url = host[name == 'mail', value],
                      username = creds[service == 'mail', login],
                      password = creds[service == 'mail', password])

## Creation bot object ----
bot <- Bot(token = tg[name == 'split_up_bot', value])

## Chasing folder in email box ----
con$select_folder('INBOX')

## Creation list of messages in today ----
msg_list <- con$search_period(since_date_char = strftime(x = Sys.Date() - 1, format = '%d-%b-%Y'),
                              before_date_char = strftime(x = Sys.Date(), format = '%d-%b-%Y')) |>
  con$fetch_body(use_uid = T, mime_level = 1L)

## Main cycle with chasing entities and sending messages in bot ----
lst <- foreach(i = 1:length(msg_list),
               .errorhandling = 'remove') %do% {
                 
                 msg <- clean_msg_text(msg_list = msg_list)[i] |>
                   as.character() |>
                   decode_mime_header() |>
                   base64enc::base64decode() |>
                   rawToChar() |>
                   utf8::as_utf8()
                 
                 words <- as.data.table(stri_extract_all_words(msg))
                 
                 colnames(words) <- c('words')
                 
                 if (any(words$words %ilike% '33shpagata|Услуга|Сумма')) {
                   
                   filial_rows <- which(words$words %ilike% '33shpagata'):(which(words$words %ilike% '33shpagata') + 1)
                   
                   abonement_rows <- (which(words$words %ilike% 'Услуга') + 1)[1]:(which(words$words %ilike% 'OrderId') - 1)
                   
                   payment_rows <- (which(words$words %ilike% 'платежа') + 1):(which(words$words %ilike% 'Сервис') - 1)
                   
                   filial <- words[filial_rows, words] |>
                     toString() |>
                     str_replace_all(pattern = ', ', replacement = ' - ')
                   
                   abonement <- words[abonement_rows, words] |>
                     toString() |>
                     str_replace_all(pattern = ', ', replacement = ' ')
                   
                   payment <- words[payment_rows, words] |>
                     toString() |>
                     str_replace_all(pattern = ', ', replacement = ' ')
                   
                   text_message <- str_glue('{filial}
                                             {abonement}
                                             {payment}')
                   
                   send_message <- bot$sendMessage(chat_id = chat[name == 'chat_id', value],
                                                   text = text_message)
                   
                 } else {
                   
                   NULL
                   
                 }
                 
               }

rm(list = ls())

gc(reset = T, full = T)