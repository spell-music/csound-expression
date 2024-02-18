import csnd6

class Control:
    def __init__(self, volume, frequency):
        engine = csnd6.Csound()
        engine.SetOption("-odac")
        engine.Compile("osc.csd") 

        thread = csnd6.CsoundPerformanceThread(engine) 
        thread.Play()              

        self.engine = engine
        self.thread = thread

        self.set_volume(volume)
        self.set_frequency(frequency)

    def set_volume(self, volume):
        self.engine.SetChannel("volume", volume)

    def set_frequency(self, frequency):
        self.engine.SetChannel("frequency", frequency)

    def close(self):
        self.thread.Stop()        
        self.thread.Join()       
        