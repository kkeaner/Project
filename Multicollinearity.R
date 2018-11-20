tried <-lm(Length ~ Source + Protocol, data=Mattrix_3)
vif(tried)
vif(tried)
#             GVIF Df GVIF^(1/(2*Df))
#Source   48.34745  5        1.473796
#Protocol 48.34745 28        1.071712
#Length as the response and source with protocol have no multicollinearity