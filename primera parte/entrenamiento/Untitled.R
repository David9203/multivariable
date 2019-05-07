library('soundecology')
>  library('pumilioR')
>  library('seewave')
> library('tuneR')
> setwd(/Users/nesdav/Downloads/pythonacustics)
Error: unexpected '/' in "setwd(/"
> setwd(""/Users/nesdav/Downloads/pythonacustics")
+ x
+ )
+ ""
Error: unexpected string constant in:
")
""
> setwd("/Users/nesdav/Downloads/pythonacustics")
> getwd()
[1] "/Users/nesdav/Downloads/pythonacustics"
> readWave("motor.wav")

Wave Object
	Number of Samples:      2635876
	Duration (seconds):     59.77
	Samplingrate (Hertz):   44100
	Channels (Mono/Stereo): Stereo
	PCM (integer format):   TRUE
	Bit (8/16/24/32/64):    16 

motor <-readWave("motor.wav")
fpeaks(meanspec(motor),amp=c(1/90,1/90))

dat=motor. ##Se cambia para hacer el analisis con cada grabacion 

data(dat)
 listen(dat)
Oscillogram<-oscillo(dat)
spectrogram1<-spectro(dat)
 spectrogram2<-spectro(dat,osc=TRUE)
mean_frequency_spectrum<-meanspec(dat)