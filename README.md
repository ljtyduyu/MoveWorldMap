# 上帝视角——给世界一个特写~

最近在研究使用R包制作动画图表，也就是类似GIF动图，感觉很有趣，也是动态图表呈现的一个非常独特的领域，刚刚研究出了些成果，今天这篇分享使用GIF动画+球型投影来制作呈现地球转动效果的动态GIF图表。<br>

过程要使用谢益辉大神的动画包——animation，该包依赖可执行程序——ImageMagic，所以导入animation包之前要提前下载并安装该动画软件。<br>

```r
library(ggplot2)
library(maps)  
library(plyr)        
library(grid)
library(showtext)
library(Cairo)
library(xlsx)
library(RColorBrewer)
library(dplyr)
library("animation")
```

本次使用maps中的世界地图素材：<br>

```r
world_map <- map_data("world")
```

为了区分大洲，我将该地图中的国家按照地理位置进行了归类（七大洲）<br>


```r
data<-read.csv("D:/R/mapdata/Word_State.csv",stringsAsFactors = FALSE,check.names = FALSE)  
ggplot(data,aes(map_id=region))+
geom_map(aes(fill=Address),map=world_map,col="white")+
expand_limits(x=world_map$long,y=world_map$lat)+
scale_y_continuous(breaks=(-2:2)*30) +
scale_x_continuous(breaks=(-4:4)*45)+
coord_map("ortho", orientation = c(30,110,0))
```

![chart1](https://github.com/ljtyduyu/MoveWorldMap/blob/master/Image/2017-05-02_101353.png)


筛选其中某一个州进行特定角度呈现： <br>

```r
data1<-data[data$Address=="North America",]
ggplot()+
geom_map(data=data,aes(map_id=region),map=world_map,col="white",fill="#A3A3A3")+
geom_map(data=data1,aes(map_id=region,fill=Address),map=world_map,col=NA)+
expand_limits(x=world_map$long,y=world_map$lat)+
scale_y_continuous(breaks=(-6:6)*15) +
scale_x_continuous(breaks=(-12:12)*15)+
coord_map("ortho", orientation = c(0,-95,0))+
guides(fill=FALSE) +
theme(
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    axis.title=element_blank(),
    panel.background=element_rect(fill="white",colour=NA),
    panel.grid.major = element_line(colour = "grey60",size=.25),
    panel.grid.minor = element_line(colour = "grey60",size=.25)
    
)
```

![chart2](https://github.com/ljtyduyu/MoveWorldMap/blob/master/Image/2017-05-02_101436.png)


```r
world_map_data<-merge(world_map,data,all.x=TRUE)
midpos<-function(x) mean(range(x,na.rm=TRUE))
centres<-ddply(world_map_data,.(Address),colwise(midpos,.(long,lat)))
centres<-centres[centres$Address!="Antarctica",]
centres$angle<-0
centres$long[centres$Address=="Asia"]=100
centres$long[centres$Address=="North America"]=-100
centres$long[centres$Address=="Oceania"]=130
```

计算每个大洲的地区中心（个别中心偏离中心大陆太远，需要手动调整）<br>

####使用grid的版面控制系统进行多图排版：

```r
setwd("E:/数据可视化/R/R语言学习笔记/可视化/Shiny/动态图表")
world_map_data<-arrange(world_map_data,group,order)
CairoPNG(file="wordmap.png",width=1600,height=1200)
showtext.begin()
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,3)))
vplayout<-function(x,y){viewport(layout.pos.row =x,layout.pos.col=y)}
for(i in 1:nrow(centres)){
mydata<-world_map_data[world_map_data$Address==centres$Address[i],]
p<-ggplot()+
geom_polygon(data=world_map_data,aes(x=long,y=lat,group=group),fill="grey95",colour="grey",size=.25)+
geom_map(data=mydata,aes(map_id=region),map=world_map_data,colour="white",fill="#F8766D",size=.25)+
coord_map("ortho",orientation=c(centres$lat[i],centres$long[i],0))+
scale_y_continuous(breaks=(-6:6)*15) +
scale_x_continuous(breaks=(-12:12)*15)+
labs(title=centres$Address[i])+
theme(
panel.background=element_rect(fill="white",colour=NA),
panel.grid.major = element_line(colour = "grey60",size=.25),
panel.grid.minor = element_line(colour = "grey60",size=.25),
text=element_text(size=20),
axis.text=element_blank(),
axis.title=element_blank(),
axis.ticks=element_blank(),
plot.title=element_text(size=20,family="myfont",hjust=.5),
plot.margin = unit(c(ifelse(i<=3,5,.5),1,ifelse(i>=3,5,.5),1),"lines")
)
print(p,vp=vplayout(ifelse(i<=3,1,2),ifelse(i<=3,i,i-3)))
}
grid.text(label="God's Perspective",x=.01,y=.98,gp=gpar(col="black",fontsize=35),draw=TRUE,just="left")
grid.text(label="Data Source:DataMofang",x=.02,y=.02,gp=gpar(col="black",fontsize=20),draw=TRUE,just="left")
showtext.end()
dev.off()
```

![chart3](https://github.com/ljtyduyu/MoveWorldMap/blob/master/Image/%E5%BE%AE%E4%BF%A1%E9%A6%96%E5%9B%BE.png)

使用animation包将361帧地图合并为GIF动画<br>
（友情提示：机器性能太弱请不要随便玩火，容易爆内存~！！！）<br>

```r
saveGIF({
ani.options(interval=.15,convert=shQuote("D:/Program Files/ImageMagick-7.0.5-Q16/convert.exe"))
for(i in 0:360){
p<-ggplot()+
geom_polygon(data=world_map_data,aes(x=long,y=lat,group=group,fill=Address),colour="grey",size=.25)+
coord_map("ortho",orientation=c(0,i,0))+
scale_y_continuous(breaks=(-6:6)*15) +
scale_x_continuous(breaks=(-12:12)*15)+
scale_fill_brewer(name="million($)",palette="Set2")+
theme(
panel.background=element_rect(fill="white",colour=NA),
panel.grid.major = element_line(colour = "grey60",size=.25),
panel.grid.minor = element_line(colour = "grey60",size=.25),
axis.text=element_blank(),
axis.title=element_blank(),
axis.ticks=element_blank()
)
print(p)
}
},movie.name='Movingworld_map.gif',ani.width=1200,ani.height=1000)
```

因为原图有13M，微信公众平台限制图片大小为5m，所以大图能就没法奉上了，不过又做了一个压缩版的小图，效果如下：<br>

```r
draw = function(i){  
ggplot()+
geom_polygon(data=world_map_data,aes(x=long,y=lat,group=group,fill=Address),colour="grey",size=.25)+
coord_map("ortho",orientation=c(0,i,0))+
scale_y_continuous(breaks=(-6:6)*15) +
scale_x_continuous(breaks=(-12:12)*15)+
scale_fill_brewer(name="million($)",palette="Set2")+
theme(
panel.background=element_rect(fill="white",colour=NA),
panel.grid.major = element_line(colour = "grey60",size=.25),
panel.grid.minor = element_line(colour = "grey60",size=.25),
axis.text=element_blank(),
axis.title=element_blank(),
axis.ticks=element_blank()
)
                  } 

oopts=ani.options(ffmpeg = "D:/Program Files/ImageMagick-7.0.5-Q16/ffmpeg.exe") 
saveVideo({  
    for(i in 1:36) print(draw(i))
    ani.options(interval = 0.6,nmax=230)},  
    video.name ="world_map_move.gif",other.opts="-b 4000k")

```

![chart4](https://github.com/ljtyduyu/MoveWorldMap/blob/master/Image/2017050214936933595907f3af54501.gif)


该案例涉及到的技术面比较广，需要使用循环、grid图形版面控制、地图投影、自编函数、颜色填充等，仅作为探索可视化道路上的一个小台阶，<br>
也许现在看起来有些高不可攀，但是当你真正深入的了解R语法以及函数编程和ggplot2之后，就没那么难理解了！<br>



联系方式：
----------------------------------------------------
wechat：ljty1991  <br>
Mail:578708965@qq.com <br>
个人公众号：数据小魔方（datamofang） <br>
团队公众号：EasyCharts <br>
qq交流群：[魔方学院]553270834

个人简介：
-------------------------------------------------
**杜雨** <br>
财经专业研究僧； <br>
伪数据可视化达人； <br>
文科背景的编程小白； <br>
喜欢研究商务图表与地理信息数据可视化，爱倒腾PowerBI、SAP DashBoard、Tableau、R ggplot2、Think-cell chart等诸如此类的数据可视化软件，创建并运营微信公众号“数据小魔方”。 <br>
Mail:578708965@qq.com <br>

<div  align="center">    
<img src="https://github.com/ljtyduyu/FontMap-of-China/blob/master/Image/resume.png" width = "550" height = "300" alt="resume" align=center />
</div>

-------------------------------------------

备注信息：
----------------------------------------------------
<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="知识共享许可协议" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />本作品采用<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">知识共享署名-非商业性使用 4.0 国际许可协议</a>进行许可。
