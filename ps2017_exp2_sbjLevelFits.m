clear all;

% Load and process data
s = readtable('morpheus3_exp2_propHappy.csv');
% s = s(2:end,:);

[~,~,subjectNum] = unique(s(:,1));

trained = strcmp(s.training_code, 'trained');
% trained = strcmp(s(:,2), '"trained"');

level = zeros(1,400);
level(strcmp(s.stimulusEmotionPercentage_code, '50%angry')) = 0;
level(strcmp(s.stimulusEmotionPercentage_code, '25%angry')) = 0.25;
level(strcmp(s.stimulusEmotionPercentage_code, '0%')) = 0.50;
level(strcmp(s.stimulusEmotionPercentage_code, '25%happy')) = 0.75;
level(strcmp(s.stimulusEmotionPercentage_code, '50%happy')) = 1.0;
% level(strcmp(s(:,3), '"50%angry"')) = 0;
% level(strcmp(s(:,3), '"25%angry"')) = 0.25;
% level(strcmp(s(:,3), '"0%"')) = 0.50;
% level(strcmp(s(:,3), '"25%happy"')) = 0.75;
% level(strcmp(s(:,3), '"50%happy"')) = 1.0;

percSaidHappy = [s.propHappy];
% percSaidHappy = [s{:,4}]; 
    
% Set up psychometric function fitting params
stimLevelsFineGrain = 0:0.01:1;
PF = @PAL_CumulativeNormal;
    
% Plotting set up
figure(1);
clf;
col = {'r', 'b'};
colModel = {[0.7 0 0], [0 0 0.7]};
for sub=1:40
   
  % For each sub, calc data, draw a plot...
%   subplot_tight(7,6,sub);
%   hold on;
  untrainedBySub(sub,:) = percSaidHappy(subjectNum==sub & trained==0);
  trainedBySub(sub,:) = percSaidHappy(subjectNum==sub & trained==1);
  
  for tr=0:1
      
    % For each training level (0=untrained,1=trained), plot the data...
    plot(level(subjectNum==sub & trained==tr), ...
      percSaidHappy(subjectNum==sub & trained==tr), col{tr+1}, 'LineWidth', tr+1);
    set(gca, 'XTickLabel', [], 'YTickLabel', []);
    
    % Separate out the levels of the psychometric function:
    levels = level(subjectNum==sub & trained==tr);
    numYes = round(percSaidHappy(subjectNum==sub & trained==tr)*30); 
    outOfNum = repmat(30, size(numYes));
    paramsFree = [1 1 0 0];  %1: free parameter, 0: fixed parameter
    
    %parameter grid defining parameter space through which to perform a
    %brute-force search for values to be used as initial guesses in iterative
    %parameter search:
    searchGrid.alpha = 0.01:0.01:1.0; % thresholds
    searchGrid.beta = logspace(0,3,101); % slopes
    searchGrid.gamma = 0.0; % no lapses
    searchGrid.lambda = 0.0; % no lapses
    
    % fit it:
    paramValues(sub,tr+1,:) = PAL_PFML_Fit(levels,numYes, ...
      outOfNum,searchGrid,paramsFree,PF);

    % plot the fit:
    proportionHappyModel(sub,tr+1,:) = PF(paramValues(sub,tr+1,:),stimLevelsFineGrain);
    plot(stimLevelsFineGrain,squeeze(proportionHappyModel(sub,tr+1,:)),'--','color',colModel{tr+1},'linewidth',2);
  end
end

% Plot mean data:
figure(2);
clf;
hold on;
plot([0 0.25 0.5 0.75 1], mean(untrainedBySub), col{1}, 'LineWidth', 2);
plot([0 0.25 0.5 0.75 1], mean(trainedBySub), col{2}, 'LineWidth', 2);

untrainedModelFit = squeeze(mean(proportionHappyModel(:,1,:),1));
trainedModelFit = squeeze(mean(proportionHappyModel(:,2,:),1));
plot(stimLevelsFineGrain,untrainedModelFit,'--','color',[0.7 0 0],'linewidth',2);
plot(stimLevelsFineGrain,trainedModelFit,'--','color',[0 0 0.7],'linewidth',2);

% Show means of these parameters:
alpha = paramValues(:,:,1); % thresh
beta = paramValues(:,:,2); % slope

figure; 
barSem({alpha(:,1), alpha(:,2)}, {'untrained', 'trained'})
figure;
barSem({beta(:,1), beta(:,2)}, {'untrained', 'trained'})

teetest(alpha(:,1), alpha(:,2))
teetest(beta(:,1), beta(:,2))


