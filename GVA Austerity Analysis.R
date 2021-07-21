# Packages ---------------------------
library("tidyverse")
setwd("~/Documents/GitHub/R/Austerity")
# Load data ---------------------------
GDP = read.csv("Data/GDP.csv")
GOV = read.csv("Data/GOV.csv")
# GDP data ---------------------------
ITL3GDP = GDP %>%
  group_by(ITL3.Region)
# GOV data ---------------------------
ITL3GOV = GOV %>%
  group_by(ITL3.Region)
# Austerity Analysis ----------
Austerity = merge(ITL3GDP, ITL3GOV, by = "ITL3.Region") %>%
  group_by(ITL3.Region) %>%
  mutate(
    change_1998_1999 = sum((X1999.y - X1998.y) / X1998.x),
    change_1999_2000 = sum((X2000.y - X1999.y) / X1999.x),
    change_2000_2001 = sum((X2001.y - X2000.y) / X2000.x),
    change_2001_2002 = sum((X2002.y - X2001.y) / X2001.x),
    change_2002_2003 = sum((X2003.y - X2002.y) / X2002.x),
    change_2003_2004 = sum((X2004.y - X2003.y) / X2003.x),
    change_2004_2005 = sum((X2005.y - X2004.y) / X2004.x),
    change_2005_2006 = sum((X2006.y - X2005.y) / X2005.x),
    change_2006_2007 = sum((X2007.y - X2006.y) / X2006.x),
    change_2007_2008 = sum((X2008.y - X2007.y) / X2007.x),
    change_2008_2009 = sum((X2009.y - X2008.y) / X2008.x),
    change_2009_2010 = sum((X2010.y - X2009.y) / X2009.x),
    change_2010_2011 = sum((X2011.y - X2010.y) / X2010.x),
    change_2011_2012 = sum((X2012.y - X2011.y) / X2011.x),
    change_2012_2013 = sum((X2013.y - X2012.y) / X2012.x),
    change_2013_2014 = sum((X2014.y - X2013.y) / X2013.x),
    change_2014_2015 = sum((X2015.y - X2014.y) / X2014.x),
    change_2015_2016 = sum((X2016.y - X2015.y) / X2015.x),
    change_2016_2017 = sum((X2017.y - X2016.y) / X2016.x),
    change_2017_2018 = sum((X2018.y - X2017.y) / X2017.x),
    change_2018_2019 = sum((X2019.y - X2018.y) / X2018.x)
  ) %>%
  select(
    ITL3.Region,
    change_1998_1999,
    change_1999_2000,
    change_2000_2001,
    change_2001_2002,
    change_2002_2003,
    change_2003_2004,
    change_2004_2005,
    change_2005_2006,
    change_2006_2007,
    change_2007_2008,
    change_2008_2009,
    change_2009_2010,
    change_2010_2011,
    change_2011_2012,
    change_2012_2013,
    change_2013_2014,
    change_2014_2015,
    change_2015_2016,
    change_2016_2017,
    change_2017_2018,
    change_2018_2019
  )

## Averages -------------

averages = Austerity %>%
  group_by(ITL3.Region) %>%
  mutate(
    average_pre_2009 = mean(
      change_1998_1999,
      change_1999_2000,
      change_2000_2001,
      change_2001_2002,
      change_2002_2003,
      change_2003_2004,
      change_2004_2005,
      change_2005_2006,
      change_2006_2007,
      change_2007_2008
    ),
    
    average_post_2009 = mean(
      change_2010_2011,
      change_2011_2012,
      change_2012_2013,
      change_2013_2014,
      change_2014_2015,
      change_2015_2016,
      change_2016_2017,
      change_2017_2018,
      change_2018_2019
    )
  ) %>%
  select(ITL3.Region, average_pre_2009, average_post_2009)

## GDP Growth -----------

GDPGrowth = ITL3GDP %>%
  group_by(ITL3.Region) %>%
  mutate(
    change_1998_1999 = sum(X1999 - X1998) / X1999,
    change_1999_2000 = sum(X2000 - X1999) / X2000,
    change_2000_2001 = sum(X2001 - X2000) / X2001,
    change_2001_2002 = sum(X2002 - X2001) / X2002,
    change_2002_2003 = sum(X2003 - X2002) / X2003,
    change_2003_2004 = sum(X2004 - X2003) / X2004,
    change_2004_2005 = sum(X2005 - X2004) / X2005,
    change_2005_2006 = sum(X2006 - X2005) / X2006,
    change_2006_2007 = sum(X2007 - X2006) / X2007,
    change_2007_2008 = sum(X2008 - X2007) / X2008,
    change_2008_2009 = sum(X2009 - X2008) / X2009,
    change_2009_2010 = sum(X2010 - X2009) / X2010,
    change_2010_2011 = sum(X2011 - X2010) / X2011,
    change_2011_2012 = sum(X2012 - X2011) / X2012,
    change_2012_2013 = sum(X2013 - X2012) / X2013,
    change_2013_2014 = sum(X2014 - X2013) / X2014,
    change_2014_2015 = sum(X2015 - X2014) / X2015,
    change_2015_2016 = sum(X2016 - X2015) / X2016,
    change_2016_2017 = sum(X2017 - X2016) / X2017,
    change_2017_2018 = sum(X2018 - X2017) / X2018,
    change_2018_2019 = sum(X2019 - X2018) / X2019
    
  ) %>%
  select(
    ITL3.Region,
    change_1998_1999,
    change_1999_2000,
    change_2000_2001,
    change_2001_2002,
    change_2002_2003,
    change_2003_2004,
    change_2004_2005,
    change_2005_2006,
    change_2006_2007,
    change_2007_2008,
    change_2008_2009,
    change_2009_2010,
    change_2010_2011,
    change_2011_2012,
    change_2012_2013,
    change_2013_2014,
    change_2014_2015,
    change_2015_2016,
    change_2016_2017,
    change_2017_2018,
    change_2018_2019
  ) 

## GDP Averages ---------

GDPAverages = GDPGrowth %>% 
  group_by(ITL3.Region) %>% 
  mutate(
    average_pre_2009 = mean(
      change_1998_1999,
      change_1999_2000,
      change_2000_2001,
      change_2001_2002,
      change_2002_2003,
      change_2003_2004,
      change_2004_2005,
      change_2005_2006,
      change_2006_2007,
      change_2007_2008
    ),
    
    average_post_2009 = mean(
      change_2010_2011,
      change_2011_2012,
      change_2012_2013,
      change_2013_2014,
      change_2014_2015,
      change_2015_2016,
      change_2016_2017,
      change_2017_2018,
      change_2018_2019
    )
  ) %>%
  select(ITL3.Region, average_pre_2009, average_post_2009)

## Write to CSV -----------

write.csv(averages, "Analysis/ITL3Averages.csv", row.names = FALSE)
write.csv(Austerity, "Analysis/ITL3AusterityData.csv", row.names = FALSE)
write.csv(GDPAverages, "Analysis/ITL3GDPAverageGrowth.csv", row.names = FALSE)
