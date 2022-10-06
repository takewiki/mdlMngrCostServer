#' 读取模板成本
#'
#' @param file_name 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
mngrCost_Read <- function(file_name = "data-raw/白标售价及管理成本20220903_upload_reviewed.xlsx",
                          dms_token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3'
                          ) {


  data <- readxl::read_excel(file_name,
                     sheet = "data")
  ncount = nrow(data)
  if(ncount >0){
    tsda::db_writeTable2(token = dms_token,table_name = 'rds_bd_mngrCost',r_object = data,append = T)
  }

  return(data)

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
