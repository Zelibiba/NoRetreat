using Avalonia;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Media.Transformation;
using Avalonia.Reactive;
using System;

namespace HexGameControls
{
    public class ScrollPane : Border
    {
        public ScrollPane()
        {
            _matrix = Matrix.Identity;

            AttachedToVisualTree += ScrollPane_AttachedToVisualTree;
            DetachedFromVisualTree += ScrollPane_DetachedFromVisualTree;

            this.GetObservable(BoundsProperty).Subscribe(new AnonymousObserver<Rect>(Rescale));
        }

        private Matrix _matrix;
        private Point _origin;
        private Point _start;

        private void SetTransformMatrix(Control control)
        {
            var tranformBuilder = new TransformOperations.Builder(1);
            tranformBuilder.AppendMatrix(_matrix);
            control.RenderTransform = tranformBuilder.Build();
        }
        private (double offsetX, double offsetY) CheckConstrains(Control control, double offsetX, double offsetY, double scaleX, double scaleY)
        {
            if (offsetX + control.Bounds.Left + control.Bounds.Width * scaleX < Bounds.Width)
                offsetX = Bounds.Width - control.Bounds.Left - control.Bounds.Width * scaleX;
            if (offsetY + control.Bounds.Top + control.Bounds.Height * scaleY < Bounds.Height)
                offsetY = Bounds.Height - control.Bounds.Top - control.Bounds.Height * scaleY;
            if (offsetX > -control.Bounds.Left) offsetX = -control.Bounds.Left;
            if (offsetY > -control.Bounds.Top) offsetY = -control.Bounds.Top;
            return (offsetX, offsetY);
        }

        private void ScrollPane_AttachedToVisualTree(object? sender, VisualTreeAttachmentEventArgs e)
        {
            if (Child == null) return;
            Child.RenderTransformOrigin = RelativePoint.TopLeft;

            PointerWheelChanged += ScrollPane_PointerWheelChanged;
            PointerPressed += ScrollPane_PointerPressed;
            PointerMoved += ScrollPane_PointerMoved;
            PointerReleased += ScrollPane_PointerReleased;
        }

        private void ScrollPane_DetachedFromVisualTree(object? sender, VisualTreeAttachmentEventArgs e)
        {
            if (Child == null) return;

            PointerWheelChanged -= ScrollPane_PointerWheelChanged;
            PointerPressed -= ScrollPane_PointerPressed;
            PointerMoved -= ScrollPane_PointerMoved;
            PointerReleased -= ScrollPane_PointerReleased;
        }

        private void Rescale(Rect bounds)
        {
            if (Child == null) return;

            var scaleX = _matrix.M11;
            var scaleY = _matrix.M22;
            var offsetX = _matrix.M31;
            var offsetY = _matrix.M32;
            if (scaleX < Bounds.Width / Child.Bounds.Width || scaleY < Bounds.Height / Child.Bounds.Height)
            {
                double zoom = Math.Max(Bounds.Width / (Child.Bounds.Width * scaleX), Bounds.Height / (Child.Bounds.Height * scaleY));
                scaleX *= zoom;
                scaleY *= zoom;
            }
            (offsetX, offsetY) = CheckConstrains(Child, offsetX, offsetY, scaleX, scaleY);
            _matrix = new Matrix(scaleX, 0, 0, scaleY, offsetX, offsetY);
            SetTransformMatrix(Child);
        }
        private void ScrollPane_PointerWheelChanged(object? sender, PointerWheelEventArgs e)
        {
            if (Child == null) return;
            var scaleX = _matrix.M11;
            var scaleY = _matrix.M22;
            var offsetX = _matrix.M31;
            var offsetY = _matrix.M32;

            var relative = e.GetPosition(Child);
            double zoom = e.Delta.Y > 0 ? 1.2 : 0.8;
            if ((scaleX * zoom < Bounds.Width / Child.Bounds.Width) || (scaleY * zoom < Bounds.Height / Child.Bounds.Height))
                zoom = Math.Max(Bounds.Width / (Child.Bounds.Width * scaleX), Bounds.Height / (Child.Bounds.Height * scaleY));

            var absX = relative.X * scaleX + offsetX;
            var absY = relative.Y * scaleY + offsetY;
            scaleX *= zoom;
            scaleY *= zoom;

            offsetX = absX - relative.X * scaleX;
            offsetY = absY - relative.Y * scaleY;
            (offsetX, offsetY) = CheckConstrains(Child, offsetX, offsetY, scaleX, scaleY);

            _matrix = new Matrix(scaleX, 0, 0, scaleY, offsetX, offsetY);
            SetTransformMatrix(Child);

            e.Handled = true;
        }

        private void ScrollPane_PointerPressed(object? sender, PointerPressedEventArgs e)
        {
            var properties = e.GetCurrentPoint(this).Properties;
            if (!properties.IsMiddleButtonPressed) return;
            if (Child == null) return;

            _origin = new(_matrix.M31, _matrix.M32);
            _start = e.GetPosition(this);

            Cursor = new(StandardCursorType.Hand);
            e.Handled = true;
        }

        private void ScrollPane_PointerMoved(object? sender, PointerEventArgs e)
        {
            var properties = e.GetCurrentPoint(this).Properties;
            if (!properties.IsMiddleButtonPressed) return;
            if (Child == null) return;

            var scaleX = _matrix.M11;
            var scaleY = _matrix.M22;

            var vector = e.GetPosition(this) - _start;
            var offsetX = _origin.X + vector.X;
            var offsetY = _origin.Y + vector.Y;
            (offsetX, offsetY) = CheckConstrains(Child, offsetX, offsetY, scaleX, scaleY);

            _matrix = new(scaleX, 0, 0, scaleY, offsetX, offsetY);
            SetTransformMatrix(Child);

            e.Handled = true;
        }
        private void ScrollPane_PointerReleased(object? sender, PointerReleasedEventArgs e)
        {
            if (Child == null) return;

            Cursor = Cursor.Default;
            e.Handled = true;
        }
    }
}
