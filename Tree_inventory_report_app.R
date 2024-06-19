library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(tidyr)
library(stringr)
library(bslib) #forgot to say - you need to install this for the themes I use

# Package ID: knb-lter-msp.2.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Minneapolis-St. Paul Urban Tree Inventory.
# Data set creator:  Adrienne Keller - Minneapolis-St. Paul Long Term Ecological Research 
# Data set creator:  Leslie Brandt - USDA Forest Service 
# Data set creator:  Jeannine Cavender-Bares - Minneapolis-St. Paul Long Term Ecological Research 
# Data set creator:  Joseph Knight - Department of Forest Resources, University of Minnesota 
# Data set creator:  Sarah Hobbie - Minneapolis-St. Paul Long Term Ecological Research 
# Contact:  Mary Marek-Spartz - Information Manager Minneapolis-St. Paul Long Term Ecological Research  - msplterdata@umn.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-msp/2/1/53c7aaf8775a237fba8297006e54a99f" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F, skip=1, sep=",",quot='"', col.names=c(
                 "entity",     
                 "givn_nm",     
                 "genus_sp",     
                 "genus",     
                 "epithet",     
                 "city",     
                 "lat",     
                 "lon",     
                 "plot_lvl",     
                 "lnd_cvr",     
                 "cvr_code"), check.names=TRUE)

unlink(infile1)

#replacing camel and snake case
dt1$entity<-gsub("([A-Z]){1}", " \\1", dt1$entity) 
dt1$entity<-str_remove(dt1$entity, " ")
dt1$entity<-str_replace_all(dt1$entity,"_", " ")


#Climate suitability index scores by tree species
CS_score<-read_tsv("Genus	species	epithet	Common Name	Planted Adapt score	Planted Adapt class	Minneapolis-vuln high emissions	Minneapolis vuln low emissions
Abies	balsamea	balsamea	Balsam fir	4.22	Medium	moderate-high	moderate-high
Abies	concolor	concolor	White fir	3.87	Medium	moderate-high	low-moderate
Abies	fraseri	fraseri		3.55	Medium	moderate-high	low-moderate
Acer	Acer saccharum subsp. nigrum	saccharum	Black maple	3.69	Medium	moderate-high	low-moderate
Acer	Acer tataricum subsp. ginnala	tataricum	Amur maple	4.5	Medium	moderate-high	low-moderate
Acer	buergerianum	buergerianum	Trident maple	4.21	Medium	moderate-high	moderate-high
Acer	campestre	campestre	Hedge maple	4.47	Medium	moderate-high	moderate-high
Acer	ginnala	ginnala		4.91	High	moderate	low
Acer	griseum	griseum	Paperbark maple	3.28	Low	high	high
Acer	Miyabe	Miyabe	Miyabei maple	5.1	High	moderate	low
Acer	miyabei	miyabei		3.6	Medium	moderate-high	low-moderate
Acer	negundo	negundo	Boxelder	4.3	Medium	moderate-high	low-moderate
Acer	nigrum	nigrum		4.12	Medium	moderate-high	moderate-high
Acer	palmatum	palmatum	Japanese maple	3.92	Medium	moderate-high	moderate-high
Acer	pensylvanicum	pensylvanicum	Striped maple	3.8	Medium	moderate-high	low-moderate
Acer	platanoides	platanoides	Norway maple	5.1	High	moderate	low
Acer	rubrum	rubrum	Red maple	4.7	Medium	low-moderate	low-moderate
Acer	saccharinum	saccharinum	Silver maple	3.8	Medium	moderate-high	low-moderate
Acer	saccharum	saccharum	Sugar maple	4.4	Medium	moderate-high	low-moderate
Acer	spicatum	spicatum	Mountain maple*	4.7	High	moderate	low
Acer	tataricum	tataricum	Tatarian maple	3.92	Medium	moderate-high	low-moderate
Acer	triflorum	triflorum		4.06	Medium	moderate-high	low-moderate
Acer	truncatum	truncatum	Shantung maple*	5.1	High	low	low
Acer	x freemanii	x freemanii	Freeman maple	4.46	High	moderate	low
Acer	x Morton Glossy	x Morton Glossy		4.01	Medium	moderate-high	low-moderate
Aesculus	flava	flava	Yellow buckeye*	3.7	Medium	moderate-high	low-moderate
Aesculus	glabra	glabra	Ohio buckeye	3.7	Medium	moderate-high	low-moderate
Aesculus	glabra	glabra		3.56	Medium	moderate-high	low-moderate
Aesculus	hippocastanum	hippocastanum	Common horsechestnut	4.2	Medium	moderate-high	low-moderate
Aesculus	x carnea	x carnea		3.82	Medium	moderate-high	low-moderate
Ailanthus	altissima	altissima	Tree of heaven	4.94	High	moderate	low
Alnus	glutinosa	glutinosa	Black alder	4.25	Medium	moderate-high	low-moderate
Alnus	hirsuta	hirsuta	Manchurian alder*	4.56	High	low	low
Alnus	incana	incana		3.52	Medium	moderate-high	moderate-high
Alnus	rugosa	rugosa		5.15	High	moderate	moderate
Amelanchier	arborea	arborea	Downy serviceberry*	5	High	low	low
Amelanchier	canadensis	canadensis	Eastern serviceberry	3.76	Medium	moderate-high	low-moderate
Amelanchier	laevis	laevis	Alleghany serviceberry	4.66	High	low	low
Amelanchier	x grandiflora	x grandiflora	apple serviceberry	4.01	Medium	moderate-high	low-moderate
Amur	maackia	maackia		3.88	Medium	moderate-high	low-moderate
Asimina	triloba	triloba	Pawpaw*	4.3	Medium	moderate-high	moderate-high
Betula	alleghaniensis	alleghaniensis	Yellow birch	4.58	High	moderate	low
Betula	lenta	lenta	Sweet birch*	4.1	Medium	low-moderate	low-moderate
Betula	nigra	nigra	River birch	3.65	Medium	low-moderate	low-moderate
Betula	papyrifera	papyrifera	Paper birch	3.65	Medium	moderate-high	low-moderate
Betula	pendula	pendula		3.9	Medium	moderate-high	low-moderate
Betula	platyphylla	platyphylla		4.17	Medium	moderate-high	low-moderate
Betula	populifolia	populifolia	Gray birch	3.22	Low	high	high
Carpinus	betulus	betulus	European Hornbeam	4.42	High	moderate	low
Carpinus	caroliniana	caroliniana	Musclewood or American Hornbeam	4.75	High	low	low
Carya	cordiformis	cordiformis	Bitternut hickory	3.83	Medium	low-moderate	low-moderate
Carya	glabra	glabra	Pignut hickory	3.91	Medium	moderate-high	low-moderate
Carya	illinoisensis	illinoisensis		4.01	Medium	moderate-high	low-moderate
Carya	ovata	ovata	Shagbark hickory	3.16	Low	high	moderate
Carya	tomentosa	tomentosa	Mockernut hickory	3.51	Medium	low-moderate	low-moderate
Castanea	dentata	dentata		4.7	High	moderate	low
Castanea	mollissima	mollissima		4.31	High	moderate	low
Catalpa	ovata	ovata		3.57	Medium	low-moderate	low-moderate
Catalpa	speciosa	speciosa	Northern catalpa	4.26	Medium	moderate-high	low-moderate
Cedrus	atlantica	atlantica		3.61	Medium	moderate-high	moderate-high
Celtis	occidentalis	occidentalis	Common hackberry	4.55	High	low	low
Cercidiphyllum	japonicum	japonicum	Katsura tree	3.31	Low	high	moderate
Cercis	canadensis	canadensis	Eastern redbud	3.9	Medium	low-moderate	low-moderate
Chionanthus	virginicus	virginicus	White fringetree	4.92	High	low	low
Cladrastis	kentukea	kentukea	Yellowwood	4.33	High	low	low
Cornus	alternifolia	alternifolia	Pagoda dogwood*	4.3	Medium	low-moderate	low-moderate
Cornus	florida	florida	Flowering dogwood	3.84	Medium	moderate-high	moderate-high
Cornus	kousa	kousa	Kousa dogwood	4.63	High	moderate	moderate
Cornus	mas	mas	Corneliancherry dogwood	4.06	Medium	moderate-high	low-moderate
Cornus	racemosa	racemosa		4.5	Medium	moderate-high	low-moderate
Corylus	americana	americana	American filbert	4.4	Medium	low-moderate	low-moderate
Corylus	colurna	colurna		4.17	Medium	moderate-high	low-moderate
Corylus	corluna	corluna	Turkish filbert or Turkish Hazel	4.27	High	moderate	low
Cotinus	coggygria	coggygria		3.4	Low	high	moderate
Cotinus	obovatus	obovatus	Smoketree*	4.9	High	low	low
Crataegus	crus-galli inermis	crus-galli inermis	Cockspur hawthorn	4.47	Medium	moderate-high	low-moderate
Crataegus	laevigata	laevigata		4.71	High	moderate	low
Crataegus	phaenopyrum	phaenopyrum	Washington hawthorn	4.25	Medium	moderate-high	low-moderate
Crataegus	viridis 'Winter King'	viridis 'Winter King'	Green hawthorn 'Winter King'	4.15	Medium	moderate-high	low-moderate
Diospyros	virginiana	virginiana	Common persimmon*	4.8	High	low	low
Elaeagnus	angustifolia	angustifolia		4.91	High	moderate	low
Eleagnus	angustifolia	angustifolia	Russian olive	4.95	High	moderate	low
Eucommia	ulmoides	ulmoides	Hardy rubbertree	4.69	High	moderate	low
Euonymus	alatus	alatus		2.9	Medium	moderate-high	low-moderate
Euonymus	atropurpureus	atropurpureus		3.3	Low	moderate	moderate
Fagus	grandifolia	grandifolia	American beech	3.55	Medium	low-moderate	low-moderate
Fagus	sylvatica	sylvatica	European beech*	3.8	Medium	low-moderate	low-moderate
Fraxinus	americana	americana	White ash	3.22	Low	moderate	moderate
Fraxinus	excelsior	excelsior	European ash	3.83	Medium	moderate-high	moderate-high
Fraxinus	nigra	nigra	Black ash	2.57	Low	high	moderate
Fraxinus	pennsylvanica	pennsylvanica	Green ash	3.9	Medium	low-moderate	low-moderate
Fraxinus	quadrangulata	quadrangulata	Blue ash	3.95	Medium	moderate-high	low-moderate
Ginkgo	biloba	biloba	Ginkgo	5.97	High	low	low
Gleditsia	triacanthos f. inermis	triacanthos f. inermis	Honeylocust	4.26	Medium	low-moderate	low-moderate
Gymnocladus	dioica	dioica		4.46	Medium	moderate-high	low-moderate
Gymnocladus	dioicus	dioicus	Kentucky coffeetree	4.6	High	low	low
Halesia	carolina	carolina		3.81	Medium	moderate-high	low-moderate
Hamamelis	virginiana	virginiana		4.89	High	moderate	low
Juglans	nigra	nigra	Black walnut	2.73	Low	moderate	moderate
Juglans	regia	regia		4.85	High	moderate	low
Juniperus	virginiana	virginiana		3.59	Medium	moderate-high	low-moderate
Koelreuteria	paniculata	paniculata	Goldenrain tree	4.71	High	moderate	moderate
Larix	decidua	decidua	European larch	3.67	Medium	moderate-high	moderate-high
Larix	laricina	laricina	Tamarack	3.86	Medium	moderate-high	moderate-high
Liquidambar	styraciflua	styraciflua	Sweetgum	3.49	Low	high	high
Liriodendron	tulipifera	tulipifera	Tuliptree	3.47	Low	high	high
Maackia	amurensis	amurensis	Amur maackia	4.85	High	moderate	low
Maclura	pomifera	pomifera	Osage-orange	4.46	Medium	low-moderate	low-moderate
Magnolia	acuminata	acuminata	Cucumbertree*	3.8	Medium	moderate-high	low-moderate
Magnolia	grandiflora	grandiflora		3.95	Medium	moderate-high	moderate-high
Magnolia	stellata	stellata	Star magnolia*	4.4	Medium	low-moderate	low-moderate
Magnolia	virginiana	virginiana	Sweetbay magnolia	4.25	Medium	moderate-high	moderate-high
Magnolia	x Butterflies	x Butterflies		3.6	Medium	moderate-high	low-moderate
Magnolia	x loebneri	x loebneri		3.7	Medium	moderate-high	low-moderate
Magnolia	x soulangeana	x soulangeana	Saucer magnolia	4.63	High	low	low
Malus	‘Prairifire’	x Prairifire	‘Prairiefire’ apple	4.4	Medium	moderate-high	low-moderate
Malus	domestica	domestica		3.82	Medium	low-moderate	low-moderate
Malus	Donald Wyman	x Donald Wyman	Donal wyman apple	4.25	Medium	moderate-high	low-moderate
Malus	pumila	pumila	Paradise apple	4.01	Medium	low-moderate	low-moderate
Malus	Snowdrift	Snowdrift	Snowdrift apple	4.3	Medium	moderate-high	low-moderate
Malus	spp.	spp.		4.25	Medium	moderate-high	moderate-high
Malus	Sugar Tyme®	x Sugar Tyme	Sugar Tyme® apple	4.25	Medium	moderate-high	low-moderate
Metasequoia	glyptostroboides	glyptostroboides	Dawn redwood	4.1	Medium	low-moderate	low-moderate
Morus	alba	alba	White mulberry	4.06	Medium	moderate-high	low-moderate
Morus	rubra	rubra	Red mulberry*	4	Medium	moderate-high	moderate-high
Nyssa	sylvatica	sylvatica	Black gum	4.72	High	moderate	moderate
Ostrya	virginiana	virginiana	American hophornbeam and Ironwood	5.41	High	low	low
Parrotia	persica	persica	Persian parrotia	5.47	High	moderate	moderate
Phellodendron	amurense	amurense	Amur corktree	4.46	Medium	moderate-high	low-moderate
Picea	abies	abies	Norway spruce	3.61	Medium	moderate-high	low-moderate
Picea	glauca	glauca	White spruce	4.15	Medium	moderate-high	low-moderate
Picea	omorika	omorika		3.84	High	low	low
Picea	pungens	pungens	Colorado blue spruce	3.95	Medium	moderate-high	low-moderate
Picea	rubens	rubens		3.7	Medium	low-moderate	low-moderate
Pinus	banksiana	banksiana		4.5	Medium	moderate-high	moderate-high
Pinus	cembra	cembra	Swiss stone pine	4.52	High	moderate	low
Pinus	mugo	mugo	Mugo pine	4.35	Medium	moderate-high	low-moderate
Pinus	nigra	nigra	Austrian pine	3.91	Medium	moderate-high	low-moderate
Pinus	parviflora	parviflora	Japanese white pine*	4	Medium	moderate-high	low-moderate
Pinus	ponderosa	ponderosa	Ponderosa pine	3.45	Low	high	moderate
Pinus	resinosa	resinosa	Red pine	2.7	Low	high	moderate
Pinus	strobus	strobus	Eastern white pine	2.9	Low	high	moderate
Pinus	sylvestris	sylvestris	Scots pine	4.42	Medium	moderate-high	low-moderate
Pinus	taeda	taeda		3.35	Low	moderate	moderate
Platanus	occidentalis	occidentalis	American sycamore	4.11	Medium	low-moderate	low-moderate
Platanus	x acerifolia	x acerifolia	London planetree	4.33	Medium	moderate-high	moderate-high
Populus	alba	alba		4.39	Medium	low-moderate	low-moderate
Populus	balsamifera	balsamifera	Balsam poplar*	3.3	Low	moderate	moderate
Populus	deltoides	deltoides	Eastern cottonwood	3.61	Low	moderate	moderate
Populus	grandidentata	grandidentata	Bigtooth aspen*	3.5	Medium	low-moderate	low-moderate
Populus	nigra	nigra		3.61	Medium	moderate-high	moderate-high
Populus	tremuloides	tremuloides	Quaking aspen	3.92	Medium	moderate-high	low-moderate
Prunus	Okame'	x Okame	Okame cherry	4.81	High	moderate	moderate
Prunus	americana	americana	American plum*	4.5	Medium	low-moderate	low-moderate
Prunus	armeniaca	armeniaca		4.07	Medium	moderate-high	moderate-high
Prunus	avium	avium	Sweet cherry	4.01	Medium	moderate-high	low-moderate
Prunus	cerasifera	cerasifera		3.26	High	low	low
Prunus	cerasus	cerasus	Sour cherry	3.8	Medium	moderate-high	low-moderate
Prunus	domestica	domestica		4.6	High	low	low
Prunus	maackii	maackii	Amur chokecherry*	4.54	High	low	low
Prunus	padus	padus		3.88	Medium	low-moderate	low-moderate
Prunus	pensylvanica	pensylvanica	Pin cherry*	4.1	Medium	low-moderate	low-moderate
Prunus	persica	persica		3.47	Low	moderate	moderate
Prunus	sargentii	sargentii	Sargent cherry*	3.8	Medium	low-moderate	low-moderate
Prunus	serotina	serotina	Black cherry	2.1	Low	moderate	moderate
Prunus	serrulata	serrulata	Japanese flowering cherry	3.2	Low	high	high
Prunus	subhirtella	subhirtella	Higan cherry	4	Medium	moderate-high	moderate-high
Prunus	virginiana	virginiana	Chokecherry	3.56	Medium	moderate-high	low-moderate
Prunus	x cistena	x cistena		5	High	moderate	moderate
Pseudotsuga	menziesii	menziesii		3.9	Medium	low-moderate	low-moderate
Pseudotsuga	menziesii var. glauca	menziesii var. glauca	Douglas-fir	2.7	Low	high	moderate
Pyrus	calleryana	calleryana	Callery pear	4.2	Medium	moderate-high	moderate-high
Pyrus	communis	communis		3.2	Low	high	high
Pyrus	ussuriensis	ussuriensis	Ussurian pear*	4.01	Low	high	high
Quercus	acutissima	acutissima	Sawtooth oak	5.48	High	moderate	moderate
Quercus	alba	alba	White oak	3.34	Low	high	moderate
Quercus	bicolor	bicolor	Swamp white oak	5.15	High	moderate	low
Quercus	coccinea	coccinea	Scarlet oak	3.82	Medium	low-moderate	low-moderate
Quercus	ellipsoidalis	ellipsoidalis	Northern pin oak	3.2	Low	high	moderate
Quercus	imbricaria	imbricaria	Shingle oak	4.5	High	moderate	low
Quercus	lyrata	lyrata	Overcup oak	4.63	High	moderate	moderate
Quercus	macrocarpa	macrocarpa	Bur oak	4.92	High	low	low
Quercus	macrocarpa	macrocarpa		4.54	High	low	low
Quercus	montana	montana	Chestnut oak	4.51	High	moderate	low
Quercus	muehlenbergii	muehlenbergii	Chinkapin oak	4.65	High	moderate	low
Quercus	palustris	palustris	Pin oak	3.52	Medium	moderate-high	low-moderate
Quercus	prinus	prinus		3.77	Medium	moderate-high	moderate-high
Quercus	robur	robur	English oak	3.92	Medium	moderate-high	low-moderate
Quercus	rubra	rubra	Northern red oak	4.05	Medium	low-moderate	low-moderate
Quercus	shumardii	shumardii	Shumard oak	3.99	Medium	moderate-high	moderate-high
Quercus	stellata	stellata		4.7	High	low	low
Quercus	velutina	velutina	Black oak	2.98	Low	high	moderate
Quercus	x bimundorum	x bimundorum	Prairie Stature® Hybrid Oak (‘Midwest')*	3.27	Low	moderate	moderate
Quercus	x heritage	x heritage		4.27	Medium	low-moderate	low-moderate
Quercus	x macdanielii	x macdanielii	Heritage oak*	5.19	High	low	low
Quercus	x Regal Prince	x Regal Prince		3.02	Low	moderate	moderate
Quercus	x schuettei	x schuettei	swamp bur oak	4.45	Medium	moderate-high	low-moderate
Quercus	x warei	x warei	Regal prince oak*	4.62	High	low	low
Rhamnus	cathartica	cathartica	European buckthorn*	5.86	High	low	low
Rhamnus	frangula	frangula		3.98	Medium	moderate-high	moderate-high
Rhus	typhina	typhina		4.06	Medium	moderate-high	moderate-high
Robinia	pseudoacacia	pseudoacacia	Black locust	3.91	Medium	low-moderate	low-moderate
Salix	amygdaloides	amygdaloides	Peach-leaved willow*	3.3	Low	high	high
Salix	babylonica	babylonica		5.19	High	moderate	moderate
Salix	discolor	discolor		3.66	Medium	low-moderate	low-moderate
Salix	matsudana	matsudana		5.15	High	moderate	low
Salix	nigra	nigra	Black willow*	3.06	Low	moderate	moderate
Sassafras	albidum	albidum	Sassafras*	4.1	Medium	low-moderate	low-moderate
Sorbus	alnifolia	alnifolia	Korean mountainash*	3.7	Medium	low-moderate	low-moderate
Sorbus	americana	americana	American mountainash*	4.2	Medium	low-moderate	low-moderate
Sorbus	aucuparia	aucuparia	European mountainash	3.72	Medium	moderate-high	low-moderate
Sorbus	decora	decora		5.3	High	moderate	moderate
Styphnolobium	japonicum	japonicum	Japanese pagoda-tree	5.27	High	moderate	moderate
Syringa	pekinensis	pekinensis	Peking tree lilac*	4.8	High	moderate	moderate
Syringa	reticulata	reticulata	Japanese tree lilac	4.55	High	moderate	low
Syringa	vulgaris	vulgaris		4.71	Medium	low-moderate	low-moderate
Taxodium	distichum	distichum	Baldcypress	4.9	High	low	low
Thuja	occidentalis	occidentalis	Northern white cedar, Arborvitae	4.77	High	moderate	low
Thuja	plicata	plicata		4	Medium	low-moderate	low-moderate
Tilia	americana	americana	American linden or basswood	4.38	Medium	moderate-high	low-moderate
Tilia	cordata	cordata	Littleleaf linden	5.18	High	moderate	low
Tilia	tomentosa	tomentosa	Silver linden	4.15	Medium	low-moderate	low-moderate
Tilia	x euchlora	x euchlora	Crimean linden*	5.3	High	moderate	low
Tilia	x Harvest Gold	x Harvest Gold		3.1	Low	moderate	moderate
Tsuga	canadensis	canadensis	Eastern hemlock	2.68	Low	high	moderate
Ulmus	Cathedral'	x Cathedral	Cathedral*	3.95	Medium	low-moderate	low-moderate
Ulmus	New Horizon;	x New Horizon	New Horizon elm*	4.13	Medium	low-moderate	low-moderate
Ulmus	Patriot'	x Patriot	Patriot*	5	High	moderate	moderate
Ulmus	‘Morton Glossy’ (Triumph™)	x Morton Glossy	Triumph elm	5.27	High	low	low
Ulmus	‘Morton’ (Accolade™)	x Morton Accolade	Accolade elm	4.9	High	moderate	low
Ulmus	americana	americana	American elm	4.45	Medium	low-moderate	low-moderate
Ulmus	carpinifolia	carpinifolia		5.2	High	low	low
Ulmus	davidiana	davidiana	Japanese Elm*	4.36	Medium	low-moderate	low-moderate
Ulmus	morton	morton		4.62	High	moderate	moderate
Ulmus	parvifolia	parvifolia	Lacebark elm*	5.5	High	low	low
Ulmus	patriot	patriot		5.8	High	low	low
Ulmus	propinqua	propinqua		5.15	High	moderate	moderate
Ulmus	pumila	pumila	Siberian elm	3.76	Medium	low-moderate	low-moderate
Ulmus	rubra	rubra	Slippery elm	4.25	Medium	low-moderate	low-moderate
Ulmus	x Frontier	x Frontier		3.48	Low	moderate	moderate
Viburnum	lentago	lentago		3.52	Medium	moderate-high	moderate-high
Zanthoxylum	americanum	americanum		4.36	Medium	low-moderate	low-moderate
Zelkova	serrata	serrata	Japanese zelkova	4.87	High	moderate	moderate")

un_CS<-unite(CS_score, "genus_sp", Genus, species, sep= " ")
unCShigh<-un_CS%>%filter(`Minneapolis-vuln high emissions`=="high")
unCSmodhigh<-un_CS%>%filter(`Minneapolis-vuln high emissions`=="moderate-high")
unCSmod<-un_CS%>%filter(`Minneapolis-vuln high emissions`=="moderate")
unCSlowmod<-un_CS%>%filter(`Minneapolis-vuln high emissions`=="low-moderate")
unCSlow<-un_CS%>%filter(`Minneapolis-vuln high emissions`=="low")

dt1f<-dt1%>%group_by(genus_sp)%>%summarise(count=n())%>%mutate(proportion=count/(nrow(dt1)))%>%arrange(desc(count))
dt1f<-dt1f%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCShigh$genus_sp, "high", "NA"))
dt1f2<-dt1f%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCSlow$genus_sp, "low", dt1f$`climate vulnerability`))
dt1f3<-dt1f2%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCSmod$genus_sp, "moderate", dt1f2$`climate vulnerability`))
dt1f4<-dt1f3%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCSmodhigh$genus_sp, "moderate-high", dt1f3$`climate vulnerability`))
dt1f5<-dt1f4%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCSlowmod$genus_sp, "low-moderate", dt1f4$`climate vulnerability`))

# Define UI for application - what features will the user encounter
# This maps to the structure of an HTML page
ui <- fluidPage(
  theme=bs_theme(bootswatch = "lumen"),
  titlePanel("Minneapolis-St. Paul Tree Inventory Dataset Interface"),
  tags$div("Researchers at the",
           tags$a(href="https://mspurbanlter.umn.edu","Minneapolis-St. Paul Long Term Ecological research program"),
           "are examining how urban forest species composition
influences the forest’s vulnerability to climate change and analyzing how patterns of
biodiversity and climate vulnerability relate to sociodemographic factors. With a focus on environmental
justice, researchers are assessing the degree to which environmental and social vulnerability overlap. By
sharing results with cities, land managers, and community members, this research aims to support
private residents, community groups, and governments in making urban forest management decisions.
Overall, researchers combined 44 species-specific, geo-located tree inventories spanning the seven-
county MSP metro area. The resulting publicly available database includes data in all seven counties,
with a more than 630,000 trees from more than 100 different genera.",
           tags$a(href="https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-msp.2.2",
                  "click here to view the metadata and download the full tree inventory dataset")),
  tags$br(),
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("dropdown"),
                          submitButton("Map!"),
                          leafletOutput("treeM")
                         
                        ),
                        
                        # user text input for a species to map...this probably would have looked better in the sidebar panel
                        mainPanel(
                          h4("Top six most abundant trees in the MSP metro-wide dataset"),
                  
                          tableOutput("msp_trees"),
                          h4(textOutput("test")),
                          tableOutput("trees_df"),
                          downloadButton("downloadData", "Download")
                        )),
                      
                      
             )
  

# Define server logic 
server <- function(input, output) {
  
  
  #create a dropdown menu that filters the dataframe
  output$dropdown <- renderUI({
    df<-dt1
    ents<-unique(df$entity)
    selectInput('city', 'filter by your city or organization', ents)
  })
  
  output$msp_trees<- renderTable(
    
    head(dt1f5)
  )
  # observe event will wait for the input of the dropdown to occur. The map output will remain
  # a default blank "proxy" leaflet map, until the event is observed and the map button is pushed
  
  observeEvent(input$city,{
    entity=as.character(input$city)
    dtf<-dt1[dt1$entity == entity ,]
    dtf_sort<-dtf%>%group_by(genus_sp)%>%summarise(count=n())%>%mutate(proportion=count/(nrow(dtf)))%>%arrange(desc(count))
    dt1m<-dtf_sort%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCShigh$genus_sp, "high", "NA"))
    dt1m2<-dt1m%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCSlow$genus_sp, "low", dt1m$`climate vulnerability`))
    dt1m3<-dt1m2%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCSmod$genus_sp, "moderate", dt1m2$`climate vulnerability`))
    dt1m4<-dt1m3%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCSmodhigh$genus_sp, "moderate-high", dt1m3$`climate vulnerability`))
    dt1m5<-dt1m4%>%mutate(`climate vulnerability`=if_else(genus_sp%in%unCSlowmod$genus_sp, "low-moderate", dt1m4$`climate vulnerability`))
    
    output$trees_df<-renderTable(head(dt1m5))
  })
  
  title_change <- reactive({
    input$city
    as.character(input$city)
  })
  
  
  observeEvent(input$city, { output$test <- renderText({ title_change()})})
  
  observeEvent(input$city, {
    tdf<-dt1
    entity=as.character(input$city)
    treedff<-tdf[tdf$entity == entity ,]
    leafletProxy("treeM")%>%
      clearMarkers()%>%
      addCircleMarkers(lat=as.numeric(treedff$lat), lng=as.numeric(treedff$lon),
                       popup = treedff$givn_nm, radius = 1.5, fillOpacity = 1, stroke = FALSE, color = "#9fb382")%>%
      fitBounds(lng1 = max(treedff$lon),lat1 = max(treedff$lat),
                lng2 = min(treedff$lon),lat2 = min(treedff$lat))
  })
  
  output$treeM<-renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = FALSE))%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") 
  )
  
  
  locs_map<- reactive({
    tdf<-dt1
    entity=as.character(input$city)
    treedff<-tdf[tdf$entity == entity ,]
    
    return(treedff)
  })
  
  # download button I looked up how to do on stack overflow 
  
  output$downloadData<- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
      
    },
    
    content = function(file){
      write.csv(locs_map(), file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
