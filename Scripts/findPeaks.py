from scipy import signal

def FindingPeaks(rowSums, minPeakHeight, distance):
  
  indexes = signal.find_peaks(rowSums, height=minPeakHeight, distance=distance)
  return indexes
  

