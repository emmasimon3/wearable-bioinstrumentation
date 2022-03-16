function [rr,rr_fft] = analyzeRESP(time,resp,plotsOn)
    % INPUTS: 
    % time: elapsed time (seconds)
    % resp: output from pressure sensor (voltage)
    % plotsOn: true for plots, false for no plots
    
    % OUTPUT:
    % rr: respiration rate (brpm) found from time domain data
    % rr_fft: respiration rate (brpm) found from frequency domain data

    % save orgiinal data
    time_raw = time;
    resp_raw = resp;

    % calculate fs
    fs = 21.8422;

    % remove offset
    resp = resp - mean(resp);

    % bandpass pass filter resp
    w1 = 50/60; 
    w2 = 90/60; 
    resp = bandpass(resp,[w1 w2],fs);

    % find peaks
    [pks, locs] = findpeaks(resp, time, 'MinPeakDistance', 1);
    
    % calcuate rr
    rr = length(pks)/((time(end) - time(1))/60);

    % fft
    Y = fft(resp);
    P2 = abs(Y/length(resp));
    P1 = P2(1:length(resp)/2+1);
    P1(2:end-1) = 2*P1(2:end-1);
    f = fs*(0:(length(resp)/2))/length(resp);
    
    % calcuate rrFft
    [~, i] = max(P1);
    rr_fft = f(i)*60/2;
    
    max(rr)
    max(rr_fft)

    if plotsOn
        figure % FILL IN CODE HERE to add legends, axes labels, and * for peaks
        
        subplot(3,1,1) 
        plot(time_raw,resp_raw)
        
        xlabel('Elasped Time (s)')
        ylabel('Voltage (V)')
        
        subplot(3,1,2)
        plot(time, resp)
        hold on
        plot(locs, pks, '*r')
        
        xlabel('Elasped Time (s)')
        ylabel('Filtered Voltage (V)')
        legend({'RESP', 'Sample RR (brpm): 38.4261'}, 'Location', 'northeast')
        hold off 
        
        subplot(3,1,3)
        plot(f,P1)
        hold on
        plot(1.05, max(P1), 'r*', 'LineWidth', 0.5);
        
        xlabel('Frequency (Hz)')
        ylabel('|P1(f)|')
        legend({'RESP', 'Sample RR FFT (brpm): 80.3497'}, 'Location', 'northeast')
        hold off
    end
end