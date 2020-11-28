import csnd6, os.path, time

def is_mp3(filename):
    filename, file_extension = os.path.splitext(filename)
    return file_extension == '.mp3'

class Player:
    def __init__(self):
        engine = csnd6.Csound()
        engine.SetOption("-odac")
        engine.Compile("player.csd") 

        thread = csnd6.CsoundPerformanceThread(engine) 
        thread.Play()              

        self.engine = engine
        self.thread = thread        

    def play_file_by_ext(self, ext, file):
        self.thread.InputMessage("i \"%s\" 0 -1 \"%s\"" % (ext, file))

    def stop(self):
        self.thread.InputMessage("i \"stop\" 0 0.01")
        time.sleep(0.02)

    def play(self, file):
        self.stop()        
        if is_mp3(file):            
            self.play_file_by_ext("mp3", file)
        else:
            self.play_file_by_ext("wav", file)       

    def close(self):
        self.thread.Stop()        
        self.thread.Join()         
        