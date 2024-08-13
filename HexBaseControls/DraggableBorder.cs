using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.Presenters;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Media.Transformation;
using Avalonia.Reactive;
using Avalonia.VisualTree;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HexGameControls
{
    public class DraggingStartetEventArgs : RoutedEventArgs
    {
        public DraggingStartetEventArgs(PointerEventArgs e) : base(DraggableBorder.DraggingStartedEvent)
        {
            PointerArgs = e;
        }

        public PointerEventArgs PointerArgs { get; }
    }
    public class DraggableBorder : Border
    {
        public DraggableBorder()
        {
            AttachedToVisualTree += OnAttachedToVisualTree;
            DetachedFromVisualTree += OnDetachedFromVisualTree;
        }

        #region Attached/Detached To Visual Tree
        private void OnAttachedToVisualTree(object? sender, VisualTreeAttachmentEventArgs e)
        {
            PointerPressed += OnPointerPressed;
            PointerMoved += OnPointerMoved;
        }

        private void OnDetachedFromVisualTree(object? sender, VisualTreeAttachmentEventArgs e)
        {
            PointerPressed -= OnPointerPressed;
            PointerMoved -= OnPointerMoved;
        }
        #endregion

        #region DraggingStartedEvent
        public static readonly RoutedEvent<DraggingStartetEventArgs> DraggingStartedEvent =
            RoutedEvent.Register<DraggableBorder, DraggingStartetEventArgs>(nameof(DraggingStarted), RoutingStrategies.Bubble);

        public event EventHandler<DraggingStartetEventArgs> DraggingStarted
        {
            add => AddHandler(DraggingStartedEvent, value);
            remove => RemoveHandler(DraggingStartedEvent, value);
        }

        protected virtual void OnDraggingStarted(PointerEventArgs e)
        {
            DraggingStartetEventArgs args = new(e);
            RaiseEvent(args);
        }
        #endregion

        #region SensitivityProperty
        public double Sensitivity
        {
            get => GetValue(SensitivityProperty);
            set => SetValue(SensitivityProperty, value);
        }
        public static readonly StyledProperty<double> SensitivityProperty =
            AvaloniaProperty.Register<TowerPanel, double>(nameof(Sensitivity));
        #endregion


        Point _startPoint;

        private void OnPointerPressed(object? sender, PointerPressedEventArgs e)
        {
            _startPoint = e.GetPosition(sender as Visual);
        }

        private void OnPointerMoved(object? sender, PointerEventArgs e)
        {
            if (sender is not Visual visual) return;
            var properties = e.GetCurrentPoint(visual).Properties;
            if (!properties.IsLeftButtonPressed) return;

            var vector = e.GetPosition(visual) - _startPoint;
            double distance = Math.Pow(vector.X, 2) + Math.Pow(vector.Y, 2);
            distance = Math.Sqrt(distance);
            if (distance < Sensitivity) return;

            OnDraggingStarted(e);
        }
    }
}
