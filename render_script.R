  
  path="~/Documents/MESIO/Statistical Learning/5_Modelos_basados_en_arboles/Classification-tree-model"
  rmarkdown::render(paste0(path,"/Task_1_v2.Rmd"), output_format =  c("pdf_document"
                                                        #,"html_document"
                                                        ),
                     output_file = paste0(path,"/Task1_Group3.pdf"))
  rmarkdown::render(paste0(path,"/Task_1_v2.Rmd"), output_format =  c("html_document"
                                                        #,"pdf_document"
                                                        ),
                    output_file = paste0(path,"/Task1_Group3.html"))
    