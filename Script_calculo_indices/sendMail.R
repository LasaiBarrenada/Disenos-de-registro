sendMail <- function(subject, to, from='repomicrodatos@ine.es', body, attachFile='not applicable', server='remesas.ine.es'){
  
  to <- paste0(unlist(to), collapse=' ')
  
  if (Sys.info()['sysname'] == "Linux"){
    
    if (attachFile == 'not applicable'){
      
      message <- paste0('echo "', body, '" | /usr/bin/mutt ', to, ' -s "', subject,'"') 
    
    } else {
      
      message <- paste0('echo "', body, '" | /usr/bin/mutt ', to, ' -s "', subject,'"',' -a ', attachFile)
    
    }
    system(message)
    
  } else if (Sys.info()['sysname'] == "Windows") {
    
    shell(paste0('SMTPMAIL.EXE from=',from,
                 ' to=', to,
                 ' subject="',subject,'"',
                 ' body="', body, '"',
                 ' server=',server),
          mustWork = TRUE,
          intern = TRUE)
    
  }
  
}