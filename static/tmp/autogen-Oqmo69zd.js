$(document).ready(function(){var dTable
function getDateStr(dateObj){var year=dateObj.getUTCFullYear(),month=dateObj.getUTCMonth()+1,day=dateObj.getUTCDate(),newDate=year+"-"+month+"-"+day;return newDate};var queryDate=getDateStr(new Date());console.log(queryDate)
function showCurrentQueryDate(qd){$("#myShowDate").html("预订情况: [<span style='color:#E8A02B'> <b><i>"+qd+"</i></b> </span>]")}
function setupDataTable(queryDay){console.log("start setup dataTable.");var titleColumns=[];titleColumns.push({field:"房间 / 时间",title:"房间 / 时间"});for(i=8;i<23;i++)titleColumns.push({field:i,title:"&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp "+i.toString()+"点",width:"200px"});$("#myTableDataReal").bootstrapTable({striped:true,pagination:true,showToggle:true,showColumns:true,cache:false,pageList:[5,8,10],url:"http://localhost:3000/daybookingstatus",method:"GET",contentType:"application/json",queryParams:function(params){params["queryDay"]=queryDay;console.log(params);return params},responseHandler:function(retData){return retData.dataRows},rowStyle:function(row,index){var classes=['success','info','active','warning','danger'];if(index%2===0&&index/2<classes.length)return {classes:classes[index/2],};return {}},columns:titleColumns});console.log("finish setup dataTable.")}
function destroyDataTable(){$("#myTableDataReal").bootstrapTable("destroy");$("#myTableDataReal").empty()};window.moment=Kalendae.moment;var utilDate=moment("2014-11-03","YYYY-MM-DD").add(2,'months');console.log("utildate:"+utilDate);$("#myCalendarSelectDiv").kalendae({userYearNav:false,direction:'today-future',months:3,weekStart:1,blackout:function(date){return date>=utilDate},subscribe:{'date-clicked':function(date,action){console.log("new selected date: "+date["_i"]);queryDate=date["_i"];console.log("the query date is: "+queryDate);destroyDataTable();showCurrentQueryDate(queryDate);setupDataTable(queryDate)}}});console.log("finish kalendae");setupDataTable(queryDate);showCurrentQueryDate(queryDate)})