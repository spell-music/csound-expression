import csnd6

class Audio:
    def __init__(self):
        engine = csnd6.Csound()
        engine.SetOption("-odac")
        engine.Compile("message.csd") 

        thread = csnd6.CsoundPerformanceThread(engine) 
        thread.Play()              

        self.engine = engine
        self.thread = thread        

    def play(self, delay, duration, volume, frequency):
        self.thread.InputMessage("i \"%s\" %f %f %f %f" % ("osc", delay, duration, volume, frequency))

    def close(self):
        self.thread.Stop()        
        self.thread.Join()         
        