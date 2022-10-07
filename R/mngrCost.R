#' 读取模板成本
#'
#' @param file_name 文件名
#' @param sheet 页签名
#' @param FYear 年份
#' @param FPeriod 月份
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mngrCost_Read()
mngrCost_Read <- function(file_name = "data-raw/产品管理成本模板.xlsx",
                          sheet = "data",
                          FYear=2022,
                          FPeriod=9,
                          dms_token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3'
                          ) {


  data <- readxl::read_excel(file_name,
                     sheet = sheet)
  ncount = nrow(data)

  if(ncount >0){
    #修改字段名称
    names(data) <- c('FNumber',
                     'Fname',
                     'Fmodel',
                     'FOldNumber',
                     'FPCSPerPkg',
                     'FMiddleSmallCoefficent',
                     'FBigMiddleCoefficent',
                     'FMngrCost',
                     'FItemProperty',
                     'FGroupName',
                     'FBaseUnit',
                     'FGroupNumber',
                     'FCategoryName',
                     'FPrdCategory',
                     'Fyear',
                     'Fperiod'
    )
    data$Fyear <-FYear
    data$Fperiod <- FPeriod
    #删除已经数据
    mngrCost_delete(dms_token = dms_token,FYear=FYear,FPeriod = FPeriod)
    #上传数据
    tsda::db_writeTable2(token = dms_token,table_name = 'rds_bd_mngrCost',r_object = data,append = T)
  }

  return(data)

}


#
#' 删除月份数据
#'
#' @param dms_token 口令
#' @param FYear 年份
#' @param FPeriod 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mngrCost_delete()
mngrCost_delete <-function(dms_token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',FYear=2022,FPeriod=9){

  sql <- paste0("delete  FROM [dbo].[rds_bd_mngrCost]
  where fyear= ",FYear," and fperiod = ",FPeriod,"")
  tsda::sql_update2(token = dms_token,sql_str = sql)
  res <- TRUE
  return(res)

}






#' 同步上一期数据
#'
#' @param dms_token  口令
#' @param FYear 年份
#' @param FPeriod 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mngrCost_SyncLastOne()
mngrCost_SyncLastOne <- function(dms_token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',FYear=2022,FPeriod=9
) {
  #获取上一年信息
  if(FPeriod==1){
    FPeriodLast =12
    FYearLast = FYear -1
  }else{
    FYearLast = FYear
    FPeriodLast =FPeriod -1

  }
  #删除已有数据

  sql <- paste0("insert into rds_bd_mngrCost
SELECT [FNumber]
      ,[Fname]
      ,[Fmodel]
      ,[FOldNumber]
      ,[FPCSPerPkg]
      ,[FMiddleSmallCoefficent]
      ,[FBigMiddleCoefficent]
      ,[FMngrCost]
      ,[FItemProperty]
      ,[FGroupName]
      ,[FBaseUnit]
      ,[FGroupNumber]
      ,[FCategoryName]
      ,[FPrdCategory]
      ,",FYear," as [Fyear]
      ,",FPeriod," as  [Fperiod]
  FROM [dbo].[rds_bd_mngrCost]
  where fyear= ",FYearLast," and fperiod = ",FPeriodLast,"")
  tsda::sql_update2(token = dms_token,sql_str = sql)
  res <- TRUE


  return(res)

}


#' 查询数据
#'
#' @param dms_token 口令
#' @param FYear 年份
#' @param FPeriod 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mngrCost_query()
#'
mngrCost_query <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                           FYear=2022,
                           FPeriod=8){
  sql <- paste0("SELECT  [FNumber] as 编码
      ,[Fname] as 名称
      ,[Fmodel] as  规格型号
      ,[FOldNumber] as  旧物料编码
      ,[FPCSPerPkg] as 单箱标准数量
      ,[FMiddleSmallCoefficent] as 中小包装换算比
      ,[FBigMiddleCoefficent] as 大中包装换算比
      ,[FMngrCost] as  管理成本
      ,[FItemProperty] as 物料属性
      ,[FGroupName] as 物料分组
      ,[FBaseUnit] as 基本单位
      ,[FGroupNumber] as 物料分组编码
      ,[FCategoryName] as 存货类别
      ,[FPrdCategory] as 产品大类
      ,[Fyear] as 年份
      ,[Fperiod] as 月份
  FROM  [rds_bd_mngrCost]
  where  fyear = ",FYear,"  and fperiod = ",FPeriod," ")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  return(data)

}




#' 查询数据
#'
#' @param dms_token 口令
#' @param FYear 年份
#' @param FPeriod 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mngrCost_query()
#'
mngrCost_checkExists <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                           FYear=2022,
                           FPeriod=8){
  sql <- paste0("SELECT   1
  FROM  [rds_bd_mngrCost]
  where  fyear = ",FYear,"  and fperiod = ",FPeriod," ")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  ncount =nrow(data)
  if(ncount >0){
    res <- TRUE
  }else{
    res<-FALSE
  }
  return(res)

}




#' 管理价格查询
#'
#' @param input 输入
#' @param output 输出
#' @param session  会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mngrCost_Server()
mngrCost_Server <- function(input,output,session,dms_token) {
  #查询管理成本
  var_txt_mngrCost_year <- tsui::var_text('txt_mngrCost_year')
  var_txt_mngrCost_period <- tsui::var_text('txt_mngrCost_period')

  shiny::observeEvent(input$btnmngrCost_query,{
    print('bug')

    FYear = as.integer(var_txt_mngrCost_year())
    FPeriod = as.integer(var_txt_mngrCost_period())
    data = mngrCost_query(dms_token = dms_token,FYear =FYear ,FPeriod = FPeriod)
    tsui::run_dataTable2(id = 'dataviewmngrCost_query',data=data)
    file_name =paste0('产品管理成本',FYear,'_',FPeriod,'.xlsx')
    tsui::run_download_xlsx(id = 'btnmngrCost_dl',data = data,filename = file_name)



  })

  #上传管理成本
  var_filemngrCost_upload <- tsui::var_file('filemngrCost_upload')
  var_txt_mngrCost_sheet <- tsui::var_text('txt_mngrCost_sheet')
  shiny::observeEvent(input$btnmngrCost_upload,{
    file_name = var_filemngrCost_upload()
    sheet_name = var_txt_mngrCost_sheet()
    FYear = as.integer(var_txt_mngrCost_year())
    FPeriod = as.integer(var_txt_mngrCost_period())
    mdlMngrCostServer::mngrCost_Read(file_name = file_name,sheet = sheet_name,FYear = FYear ,FPeriod = FPeriod,dms_token = dms_token)

    tsui::pop_notice('产品管理成本上传成功')


  })

  #同步上月管理成本
  shiny::observeEvent(input$btnmngrCost_syncLastOne,{
    FYear = as.integer(var_txt_mngrCost_year())
    FPeriod = as.integer(var_txt_mngrCost_period())
    mngrCost_SyncLastOne(dms_token = dms_token,FYear =FYear ,FPeriod = FPeriod)
    tsui::pop_notice('产品管理成本同步成功')




  })







}
