library(sf)
library(leaflet)
library(terra)

# Change these to your data sources
dir_jules<-"/Users/jonathanmosedale/Library/CloudStorage/OneDrive-UniversityofExeter/Add_Trees/Jules_output"
parcels.shp<-"/Users/jonathanmosedale/Library/CloudStorage/OneDrive-UniversityofExeter/Add_Trees/Elicitor_App/land_parcels.shp"


#########################  Load data & rename variables ######################### 
parcels_sf<-st_read(parcels.shp)[,1] # read only parcel id and geometry
names(parcels_sf)<-c('Parcel.ID','geometry')
centre_pt<-as.numeric(st_coordinates(st_centroid(st_as_sfc(st_bbox(parcels_sf)))))

cveg_df<-read.csv(file.path(dir_jules,"c_veg_all.csv"))
names(cveg_df)<-c('Parcel.ID','vegC_tCha')
npp_df<-read.csv(file.path(dir_jules,"npp_gb_all.csv"))
names(npp_df)<-c('Parcel.ID','NPP_avg2040s')


#########################  Merge ######################### 
cveg_sf<-merge(parcels_sf,cveg_df)
npp_sf<-merge(parcels_sf,npp_df)


######################### Mapping ######################### 
# Labels
cveg<-'Vegetation carbon in 2050 (tC/ha)'
npp<-'Average NPP per parcel (gC/m2/year)'

## Breaks, scales and palettes
step<-25
cveg_brks <- seq(min(cveg_sf$vegC_tCha)%/%step*step, (1+max(cveg_sf$vegC_tCha)%/%step)*step, by=step)
cveg_pal<-colorBin('BuGn',domain=c(cveg_brks[1],cveg_brks[length(cveg_brks)]),bins=cveg_brks,na.color="transparent",reverse=FALSE)

step<-50
npp_brks <- seq(min(npp_sf$NPP_avg2040s)%/%step*step, (1+max(npp_sf$NPP_avg2040s)%/%step)*step, by=step)
npp_pal<-colorBin('viridis',domain=c(npp_brks[1],npp_brks[length(npp_brks)]),bins=npp_brks,na.color="transparent",reverse=FALSE)

# Popup info
popup_text <- paste0("<strong>Parcel ID: </strong>",  cveg_sf$Parcel.ID, 
                      "<br><strong>Vegetation carbon 2050 tC/ha: </strong>", round(cveg_sf$vegC_tCha,1),
                     "<br><strong>Average NPP per parcel gC/m2/year: </strong>", round(npp_df$NPP_avg2040s,1))

# Map 
# map<- # save to object
leaflet(options=list(minZoom=8)) %>%
  setView(lng = centre_pt[1], lat = centre_pt[2], zoom = 12) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik,group="Streetmap") %>%
  addPolygons(data=cveg_sf,color='black',fillColor=cveg_pal(cveg_sf$vegC_tCha), 
              weight = 0.5, fillOpacity = 0.7, opacity = 1,group=cveg, 
              popup = popup_text, 
              highlightOptions = highlightOptions(color = "yellow",weight = 2, 
                                                  fillColor = 'yellow', bringToFront = TRUE)) %>%
  addPolygons(data=npp_sf,color='black',fillColor=npp_pal(npp_sf$NPP_avg2040s), 
              weight = 0.5, fillOpacity = 0.7, opacity = 1,group=npp,
              popup = popup_text, 
              highlightOptions = highlightOptions(color = "yellow",weight = 2, 
                                                  fillColor = 'yellow', bringToFront = TRUE)) %>%
  
  addLegend(title=cveg, group = cveg, position = "bottomleft", pal=cveg_pal,values=c(cveg_brks[1],cveg_brks[length(cveg_brks)])) %>%
  addLegend(title=npp, group = npp, position = "bottomleft", pal=npp_pal,values=c(npp_brks[1],npp_brks[length(npp_brks)])) %>%
  addLayersControl(baseGroups=c(cveg,npp)) %>%
  
# Hack to show only one legend at a time: see https://stackoverflow.com/questions/52393310/is-it-possible-to-switch-between-multiple-legends-when-switching-between-base-gr
  htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }")

# To save as html widget:
#htmlwidgets::saveWidget(map, file = file.path(dir_jules,"index.html"))
